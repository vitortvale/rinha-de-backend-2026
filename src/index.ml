module BA = Bigarray
module A1 = Bigarray.Array1
module Std_unix = Unix
open Core

type t =
  { vectors : (int, BA.int16_unsigned_elt, BA.c_layout) A1.t
  ; labels : (int, BA.int8_unsigned_elt, BA.c_layout) A1.t
  ; rows : (int, BA.int8_unsigned_elt, BA.c_layout) A1.t
  ; kinds : (int, BA.int8_unsigned_elt, BA.c_layout) A1.t
  ; pivots : (int, BA.int8_unsigned_elt, BA.c_layout) A1.t
  ; radii : float array
  ; lefts : (int, BA.int8_unsigned_elt, BA.c_layout) A1.t
  ; rights : (int, BA.int8_unsigned_elt, BA.c_layout) A1.t
  ; starts : (int, BA.int8_unsigned_elt, BA.c_layout) A1.t
  ; counts : (int, BA.int8_unsigned_elt, BA.c_layout) A1.t
  ; node_count : int
  ; centroid_ivf_index : (int, BA.int8_unsigned_elt, BA.c_layout) A1.t
  ; centroid_ivf_centroids : float array
  ; centroid_ivf_offsets : int array
  ; centroid_ivf_k : int
  ; centroid_ivf_total_blocks : int
  ; ivf_nprobe : int
  ; ivf_fast_nprobe : int
  }

let reference_rows = 3_000_000
let k = 5

let validate_size path expected_bytes =
  let actual_bytes = (Std_unix.stat path).st_size in
  if actual_bytes <> expected_bytes
  then
    failwithf
      "bad packaged reference size for %s: got %d bytes, expected %d"
      path
      actual_bytes
      expected_bytes
      ()
;;

let map_file path kind len =
  let fd = Std_unix.openfile path [ Std_unix.O_RDONLY ] 0 in
  let mapped = Std_unix.map_file fd kind BA.c_layout false [| len |] in
  Std_unix.close fd;
  BA.array1_of_genarray mapped
;;

let map_i32 path len = map_file path BA.int32 len
let i32_get a i = A1.unsafe_get a i |> Stdlib.Int32.to_int
let i64_get a i = A1.unsafe_get a i |> Int64.to_int_exn

let int_env name default =
  match Sys.getenv name with
  | None -> default
  | Some value -> Int.of_string value
;;

let map_i32_bytes path len = map_file path BA.int8_unsigned (len * 4)

let i32_bytes_get a i =
  let base = i lsl 2 in
  let b0 = A1.unsafe_get a base in
  let b1 = A1.unsafe_get a (base + 1) in
  let b2 = A1.unsafe_get a (base + 2) in
  let b3 = A1.unsafe_get a (base + 3) in
  let unsigned = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) in
  if b3 land 0x80 = 0 then unsigned else unsigned - 0x1_0000_0000
;;

let load data_dir =
  let vectors_path = Filename.concat data_dir "references.u16" in
  let labels_path = Filename.concat data_dir "labels.u8" in
  let rows_path = Filename.concat data_dir "vp_rows.i32" in
  let kind_path = Filename.concat data_dir "vp_kind.u8" in
  let pivot_path = Filename.concat data_dir "vp_pivot.i32" in
  let radius_path = Filename.concat data_dir "vp_radius.i64" in
  let left_path = Filename.concat data_dir "vp_left.i32" in
  let right_path = Filename.concat data_dir "vp_right.i32" in
  let start_path = Filename.concat data_dir "vp_start.i32" in
  let count_path = Filename.concat data_dir "vp_count.i32" in
  let centroid_ivf_index_path = Filename.concat data_dir "centroid_ivf_index.bin" in
  validate_size vectors_path (reference_rows * Vectorize.dim * 2);
  validate_size labels_path reference_rows;
  validate_size rows_path (reference_rows * 4);
  let node_count = (Std_unix.stat kind_path).st_size in
  validate_size pivot_path (node_count * 4);
  validate_size radius_path (node_count * 8);
  validate_size left_path (node_count * 4);
  validate_size right_path (node_count * 4);
  validate_size start_path (node_count * 4);
  validate_size count_path (node_count * 4);
  let radii =
    let radii_raw = map_file radius_path BA.int64 node_count in
    Array.init node_count ~f:(fun i -> Stdlib.sqrt (Float.of_int (i64_get radii_raw i)))
  in
  let centroid_ivf_index_len = (Std_unix.stat centroid_ivf_index_path).st_size in
  let centroid_ivf_index =
    map_file centroid_ivf_index_path BA.int8_unsigned centroid_ivf_index_len
  in
  let centroid_ivf_u32 offset =
    let b0 = A1.unsafe_get centroid_ivf_index offset in
    let b1 = A1.unsafe_get centroid_ivf_index (offset + 1) in
    let b2 = A1.unsafe_get centroid_ivf_index (offset + 2) in
    let b3 = A1.unsafe_get centroid_ivf_index (offset + 3) in
    b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)
  in
  let centroid_ivf_i32_bits offset =
    let open Stdlib.Int32 in
    logor
      (of_int (A1.unsafe_get centroid_ivf_index offset))
      (logor
         (shift_left (of_int (A1.unsafe_get centroid_ivf_index (offset + 1))) 8)
         (logor
            (shift_left (of_int (A1.unsafe_get centroid_ivf_index (offset + 2))) 16)
            (shift_left (of_int (A1.unsafe_get centroid_ivf_index (offset + 3))) 24)))
  in
  if not (Char.equal (Char.of_int_exn (A1.unsafe_get centroid_ivf_index 0)) 'I')
  then failwith "bad centroid_ivf index";
  let centroid_ivf_n = centroid_ivf_u32 4 in
  let centroid_ivf_k = centroid_ivf_u32 8 in
  let centroid_ivf_dim = centroid_ivf_u32 12 in
  if centroid_ivf_n <> reference_rows
     || centroid_ivf_k <> 4096
     || centroid_ivf_dim <> Vectorize.dim
  then failwith "bad centroid_ivf index metadata";
  let centroid_ivf_offsets_base = 16 + (Vectorize.dim * centroid_ivf_k * 4) in
  let centroid_ivf_total_blocks =
    centroid_ivf_u32 (centroid_ivf_offsets_base + (centroid_ivf_k * 4))
  in
  let centroid_ivf_centroids =
    Array.init (Vectorize.dim * centroid_ivf_k) ~f:(fun i ->
      Int32.float_of_bits (centroid_ivf_i32_bits (16 + (i * 4))))
  in
  let centroid_ivf_offsets =
    Array.init (centroid_ivf_k + 1) ~f:(fun centroid ->
      centroid_ivf_u32 (centroid_ivf_offsets_base + (centroid * 4)))
  in
  Gc.full_major ();
  { vectors = map_file vectors_path BA.int16_unsigned (reference_rows * Vectorize.dim)
  ; labels = map_file labels_path BA.int8_unsigned reference_rows
  ; rows = map_i32_bytes rows_path reference_rows
  ; kinds = map_file kind_path BA.int8_unsigned node_count
  ; pivots = map_i32_bytes pivot_path node_count
  ; radii
  ; lefts = map_i32_bytes left_path node_count
  ; rights = map_i32_bytes right_path node_count
  ; starts = map_i32_bytes start_path node_count
  ; counts = map_i32_bytes count_path node_count
  ; node_count
  ; centroid_ivf_index
  ; centroid_ivf_centroids
  ; centroid_ivf_offsets
  ; centroid_ivf_k
  ; centroid_ivf_total_blocks
  ; ivf_nprobe = Int.clamp_exn (int_env "IVF_NPROBE" 24) ~min:1 ~max:centroid_ivf_k
  ; ivf_fast_nprobe =
      Int.clamp_exn (int_env "IVF_FAST_NPROBE" 8) ~min:1 ~max:centroid_ivf_k
  }
;;

let row_distance t query row limit =
  let vectors = t.vectors in
  let base = row * Vectorize.dim in
  let d0 = query.(0) - A1.unsafe_get vectors base in
  let acc = d0 * d0 in
  if acc >= limit
  then acc
  else (
    let d1 = query.(1) - A1.unsafe_get vectors (base + 1) in
    let acc = acc + (d1 * d1) in
    if acc >= limit
    then acc
    else (
      let d2 = query.(2) - A1.unsafe_get vectors (base + 2) in
      let acc = acc + (d2 * d2) in
      if acc >= limit
      then acc
      else (
        let d3 = query.(3) - A1.unsafe_get vectors (base + 3) in
        let acc = acc + (d3 * d3) in
        if acc >= limit
        then acc
        else (
          let d4 = query.(4) - A1.unsafe_get vectors (base + 4) in
          let acc = acc + (d4 * d4) in
          if acc >= limit
          then acc
          else (
            let d5 = query.(5) - A1.unsafe_get vectors (base + 5) in
            let acc = acc + (d5 * d5) in
            if acc >= limit
            then acc
            else (
              let d6 = query.(6) - A1.unsafe_get vectors (base + 6) in
              let acc = acc + (d6 * d6) in
              if acc >= limit
              then acc
              else (
                let d7 = query.(7) - A1.unsafe_get vectors (base + 7) in
                let acc = acc + (d7 * d7) in
                if acc >= limit
                then acc
                else (
                  let d8 = query.(8) - A1.unsafe_get vectors (base + 8) in
                  let acc = acc + (d8 * d8) in
                  if acc >= limit
                  then acc
                  else (
                    let d9 = query.(9) - A1.unsafe_get vectors (base + 9) in
                    let acc = acc + (d9 * d9) in
                    if acc >= limit
                    then acc
                    else (
                      let d10 = query.(10) - A1.unsafe_get vectors (base + 10) in
                      let acc = acc + (d10 * d10) in
                      if acc >= limit
                      then acc
                      else (
                        let d11 = query.(11) - A1.unsafe_get vectors (base + 11) in
                        let acc = acc + (d11 * d11) in
                        if acc >= limit
                        then acc
                        else (
                          let d12 = query.(12) - A1.unsafe_get vectors (base + 12) in
                          let acc = acc + (d12 * d12) in
                          if acc >= limit
                          then acc
                          else (
                            let d13 = query.(13) - A1.unsafe_get vectors (base + 13) in
                            acc + (d13 * d13))))))))))))))
;;

let add_candidate_dist t best_dist best_label best_tau row dist =
  if dist < best_dist.(k - 1)
  then (
    let rec insertion_pos pos =
      if pos > 0 && dist < best_dist.(pos - 1)
      then (
        best_dist.(pos) <- best_dist.(pos - 1);
        best_label.(pos) <- best_label.(pos - 1);
        insertion_pos (pos - 1))
      else pos
    in
    let pos = insertion_pos (k - 1) in
    best_dist.(pos) <- dist;
    best_label.(pos) <- A1.unsafe_get t.labels row;
    best_tau := Stdlib.sqrt (Float.of_int best_dist.(k - 1)))
;;

let add_candidate t query best_dist best_label best_tau row =
  let dist = row_distance t query row best_dist.(k - 1) in
  add_candidate_dist t best_dist best_label best_tau row dist
;;

let frauds_of_labels best_label =
  best_label.(0) + best_label.(1) + best_label.(2) + best_label.(3) + best_label.(4)
;;

let centroid_ivf_centroids_base = 16

let centroid_ivf_offsets_base t =
  centroid_ivf_centroids_base + (Vectorize.dim * t.centroid_ivf_k * 4)
;;

let centroid_ivf_labels_base t = centroid_ivf_offsets_base t + ((t.centroid_ivf_k + 1) * 4)

let centroid_ivf_blocks_base t =
  centroid_ivf_labels_base t + (t.centroid_ivf_total_blocks * 8)
;;

let centroid_ivf_i16 t offset =
  let b0 = A1.unsafe_get t.centroid_ivf_index offset in
  let b1 = A1.unsafe_get t.centroid_ivf_index (offset + 1) in
  let unsigned = b0 lor (b1 lsl 8) in
  if b1 land 0x80 = 0 then unsigned else unsigned - 0x1_0000
;;

let centroid_ivf_offset t centroid = t.centroid_ivf_offsets.(centroid)

let centroid_ivf_centroid t dim centroid =
  t.centroid_ivf_centroids.((dim * t.centroid_ivf_k) + centroid)
;;

let centroid_ivf_top_centroids t query_float n =
  let top_dist = Array.create ~len:n Float.infinity in
  let top_idx = Array.create ~len:n 0 in
  for centroid = 0 to t.centroid_ivf_k - 1 do
    let acc = ref 0. in
    for dim = 0 to Vectorize.dim - 1 do
      let diff = centroid_ivf_centroid t dim centroid -. query_float.(dim) in
      acc := !acc +. (diff *. diff)
    done;
    let dist = !acc in
    if Stdlib.( < ) dist top_dist.(n - 1)
    then (
      let rec insert pos =
        if pos > 0 && Stdlib.( < ) dist top_dist.(pos - 1)
        then (
          top_dist.(pos) <- top_dist.(pos - 1);
          top_idx.(pos) <- top_idx.(pos - 1);
          insert (pos - 1))
        else pos
      in
      let pos = insert (n - 1) in
      top_dist.(pos) <- dist;
      top_idx.(pos) <- centroid)
  done;
  top_idx
;;

let centroid_ivf_add_candidate t best_dist best_label label_base slot dist =
  if dist < best_dist.(k - 1)
  then (
    let label = A1.unsafe_get t.centroid_ivf_index (label_base + slot) in
    let rec insert pos =
      if pos > 0 && dist < best_dist.(pos - 1)
      then (
        best_dist.(pos) <- best_dist.(pos - 1);
        best_label.(pos) <- best_label.(pos - 1);
        insert (pos - 1))
      else pos
    in
    let pos = insert (k - 1) in
    best_dist.(pos) <- dist;
    best_label.(pos) <- label)
;;

let centroid_ivf_slot_distance t query block_base slot limit =
  let rec loop dim acc =
    if acc >= limit
    then acc
    else if dim = Vectorize.dim
    then acc
    else (
      let value = centroid_ivf_i16 t (block_base + (((dim * 8) + slot) * 2)) in
      let diff = value - (query.(dim) - 10_000) in
      loop (dim + 1) (acc + (diff * diff)))
  in
  loop 0 0
;;

let centroid_ivf_scan_probe t query best_dist best_label centroid =
  let start_block = centroid_ivf_offset t centroid in
  let stop_block = centroid_ivf_offset t (centroid + 1) in
  let blocks_base = centroid_ivf_blocks_base t in
  let labels_base = centroid_ivf_labels_base t in
  for block = start_block to stop_block - 1 do
    let block_base = blocks_base + (block * 112 * 2) in
    let label_base = labels_base + (block * 8) in
    for slot = 0 to 7 do
      let dist = centroid_ivf_slot_distance t query block_base slot best_dist.(k - 1) in
      centroid_ivf_add_candidate t best_dist best_label label_base slot dist
    done
  done
;;

let score_frauds_centroid_ivf_nprobe t query nprobe =
  let query_float =
    Array.init Vectorize.dim ~f:(fun dim -> Float.of_int (query.(dim) - 10_000) *. 0.0001)
  in
  let probes = centroid_ivf_top_centroids t query_float nprobe in
  let best_dist = Array.create ~len:k Int.max_value in
  let best_label = Array.create ~len:k 0 in
  for i = 0 to Array.length probes - 1 do
    centroid_ivf_scan_probe t query best_dist best_label probes.(i)
  done;
  frauds_of_labels best_label
;;

let score_frauds_centroid_ivf t query =
  let fast = score_frauds_centroid_ivf_nprobe t query t.ivf_fast_nprobe in
  if fast <> 2 && fast <> 3
  then fast
  else score_frauds_centroid_ivf_nprobe t query t.ivf_nprobe
;;

let prewarm t =
  let checksum = ref 0 in
  for base = 0 to (reference_rows - 1) * Vectorize.dim do
    checksum := !checksum + A1.unsafe_get t.vectors base
  done;
  for row = 0 to reference_rows - 1 do
    checksum := !checksum + A1.unsafe_get t.labels row + i32_bytes_get t.rows row
  done;
  for node = 0 to t.node_count - 1 do
    checksum
    := !checksum
       + A1.unsafe_get t.kinds node
       + i32_bytes_get t.pivots node
       + i32_bytes_get t.lefts node
       + i32_bytes_get t.rights node
       + i32_bytes_get t.starts node
       + i32_bytes_get t.counts node
       + Int.of_float t.radii.(node)
  done;
  let rec touch_index offset =
    if offset < A1.dim t.centroid_ivf_index
    then (
      checksum := !checksum + A1.unsafe_get t.centroid_ivf_index offset;
      touch_index (offset + 4096))
  in
  touch_index 0;
  Sys.opaque_identity !checksum |> ignore;
  ()
;;

let score_frauds_vp t query =
  let best_dist = Array.create ~len:k Int.max_value in
  let best_label = Array.create ~len:k 0 in
  let best_tau = ref Float.infinity in
  let rec search node =
    if node >= 0 && node < t.node_count
    then (
      match A1.unsafe_get t.kinds node with
      | 0 ->
        let start = i32_bytes_get t.starts node in
        let count = i32_bytes_get t.counts node in
        for pos = start to start + count - 1 do
          add_candidate t query best_dist best_label best_tau (i32_bytes_get t.rows pos)
        done
      | _ ->
        let pivot = i32_bytes_get t.pivots node in
        let dist_sq = row_distance t query pivot Int.max_value in
        add_candidate_dist t best_dist best_label best_tau pivot dist_sq;
        let dist = Float.sqrt (Float.of_int dist_sq) in
        let radius = t.radii.(node) in
        let left = i32_bytes_get t.lefts node in
        let right = i32_bytes_get t.rights node in
        if Float.(dist < radius)
        then (
          search left;
          if Float.(dist + !best_tau >= radius) then search right)
        else (
          search right;
          if Float.(dist - !best_tau <= radius) then search left))
  in
  search 0;
  frauds_of_labels best_label
;;

let score_frauds t query = score_frauds_centroid_ivf t query
let score t query = Float.of_int (score_frauds t query) /. Float.of_int k
