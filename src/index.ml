module BA = Bigarray
module A1 = Bigarray.Array1
module Std_unix = Unix
open Core

type vp =
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
  }

type t =
  { vp : vp option
  ; centroid_ivf_labels : (int, BA.int8_unsigned_elt, BA.c_layout) A1.t
  ; centroid_ivf_blocks_i16 : (int, BA.int16_signed_elt, BA.c_layout) A1.t
  ; centroid_ivf_centroids : float array
  ; centroid_ivf_offsets : int array
  ; centroid_ivf_label_base : int
  ; centroid_ivf_block_base_i16 : int
  ; centroid_ivf_k : int
  ; centroid_ivf_total_blocks : int
  ; ivf_nprobe : int
  ; ivf_fast_nprobe : int
  ; ivf_verify_nprobe : int
  }

type scorer =
  { top_dist : float array
  ; top_idx : int array
  ; best_dist : int array
  ; best_label : int array
  }

let reference_rows = 3_000_000
let k = 5

let create_scorer t =
  let max_nprobe = Int.max t.ivf_verify_nprobe (Int.max t.ivf_nprobe t.ivf_fast_nprobe) in
  { top_dist = Array.create ~len:max_nprobe Float.infinity
  ; top_idx = Array.create ~len:max_nprobe 0
  ; best_dist = Array.create ~len:k Int.max_value
  ; best_label = Array.create ~len:k 0
  }
;;

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

let load_vp data_dir =
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
  }
;;

let load_index ?(with_vp = false) data_dir =
  let centroid_ivf_index_path = Filename.concat data_dir "centroid_ivf_index.bin" in
  let centroid_ivf_meta_path = Filename.concat data_dir "centroid_ivf_meta.bin" in
  let centroid_ivf_labels_path = Filename.concat data_dir "centroid_ivf_labels.u8" in
  let centroid_ivf_blocks_path = Filename.concat data_dir "centroid_ivf_blocks.i16" in
  let split_index =
    Stdlib.Sys.file_exists centroid_ivf_meta_path
    && Stdlib.Sys.file_exists centroid_ivf_labels_path
    && Stdlib.Sys.file_exists centroid_ivf_blocks_path
  in
  let centroid_ivf_index_len =
    (Std_unix.stat (if split_index then centroid_ivf_meta_path else centroid_ivf_index_path))
      .st_size
  in
  let centroid_ivf_index =
    map_file
      (if split_index then centroid_ivf_meta_path else centroid_ivf_index_path)
      BA.int8_unsigned
      centroid_ivf_index_len
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
  let centroid_ivf_labels_base = centroid_ivf_offsets_base + ((centroid_ivf_k + 1) * 4) in
  if split_index && centroid_ivf_index_len <> centroid_ivf_labels_base
  then failwith "bad centroid_ivf meta size";
  let centroid_ivf_total_blocks =
    centroid_ivf_u32 (centroid_ivf_offsets_base + (centroid_ivf_k * 4))
  in
  let
    ( centroid_ivf_labels
    , centroid_ivf_blocks_i16
    , centroid_ivf_label_base
    , centroid_ivf_block_base_i16 )
    =
    if split_index
    then (
      validate_size centroid_ivf_labels_path (centroid_ivf_total_blocks * 8);
      validate_size centroid_ivf_blocks_path (centroid_ivf_total_blocks * 14 * 8 * 2);
      ( map_file
          centroid_ivf_labels_path
          BA.int8_unsigned
          (centroid_ivf_total_blocks * 8)
      , map_file
          centroid_ivf_blocks_path
          BA.int16_signed
          (centroid_ivf_total_blocks * 14 * 8)
      , 0
      , 0 ))
    else (
      if centroid_ivf_index_len land 1 <> 0 then failwith "bad centroid_ivf index size";
      ( centroid_ivf_index
      , map_file centroid_ivf_index_path BA.int16_signed (centroid_ivf_index_len lsr 1)
      , centroid_ivf_labels_base
      , (centroid_ivf_labels_base + (centroid_ivf_total_blocks * 8)) lsr 1 ))
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
  { vp = (if with_vp then Some (load_vp data_dir) else None)
  ; centroid_ivf_labels
  ; centroid_ivf_blocks_i16
  ; centroid_ivf_centroids
  ; centroid_ivf_offsets
  ; centroid_ivf_label_base
  ; centroid_ivf_block_base_i16
  ; centroid_ivf_k
  ; centroid_ivf_total_blocks
  ; ivf_nprobe = Int.clamp_exn (int_env "IVF_NPROBE" 24) ~min:1 ~max:centroid_ivf_k
  ; ivf_fast_nprobe =
      Int.clamp_exn (int_env "IVF_FAST_NPROBE" 3) ~min:1 ~max:centroid_ivf_k
  ; ivf_verify_nprobe =
      Int.clamp_exn
        (int_env "IVF_VERIFY_NPROBE" (int_env "IVF_NPROBE" 24))
        ~min:1
        ~max:centroid_ivf_k
  }
;;

let load data_dir = load_index data_dir
let load_for_bench data_dir = load_index ~with_vp:true data_dir

let row_distance vp query row limit =
  let vectors = vp.vectors in
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

let add_candidate_dist vp best_dist best_label best_tau row dist =
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
    best_label.(pos) <- A1.unsafe_get vp.labels row;
    best_tau := Stdlib.sqrt (Float.of_int best_dist.(k - 1)))
;;

let add_candidate vp query best_dist best_label best_tau row =
  let dist = row_distance vp query row best_dist.(k - 1) in
  add_candidate_dist vp best_dist best_label best_tau row dist
;;

let[@zero_alloc] [@inline always] frauds_of_labels (best_label @ local) =
  best_label.(0) + best_label.(1) + best_label.(2) + best_label.(3) + best_label.(4)
;;

let centroid_ivf_centroids_base = 16

let centroid_ivf_offsets_base t =
  centroid_ivf_centroids_base + (Vectorize.dim * t.centroid_ivf_k * 4)
;;

let centroid_ivf_offset t centroid = t.centroid_ivf_offsets.(centroid)

let[@zero_alloc] reset_best (best_dist @ local) (best_label @ local) =
  for i = 0 to k - 1 do
    best_dist.(i) <- Int.max_value;
    best_label.(i) <- 0
  done
;;

let[@zero_alloc] centroid_ivf_top_centroids
  t
  (top_dist @ local)
  (top_idx @ local)
  q0
  q1
  q2
  q3
  q4
  q5
  q6
  q7
  q8
  q9
  q10
  q11
  q12
  q13
  n
  =
  let centroids = t.centroid_ivf_centroids in
  let centroid_k = t.centroid_ivf_k in
  let c1_base = centroid_k in
  let c2_base = c1_base + centroid_k in
  let c3_base = c2_base + centroid_k in
  let c4_base = c3_base + centroid_k in
  let c5_base = c4_base + centroid_k in
  let c6_base = c5_base + centroid_k in
  let c7_base = c6_base + centroid_k in
  let c8_base = c7_base + centroid_k in
  let c9_base = c8_base + centroid_k in
  let c10_base = c9_base + centroid_k in
  let c11_base = c10_base + centroid_k in
  let c12_base = c11_base + centroid_k in
  let c13_base = c12_base + centroid_k in
  let q0 = Float.of_int q0 *. 0.0001 in
  let q1 = Float.of_int q1 *. 0.0001 in
  let q2 = Float.of_int q2 *. 0.0001 in
  let q3 = Float.of_int q3 *. 0.0001 in
  let q4 = Float.of_int q4 *. 0.0001 in
  let q5 = Float.of_int q5 *. 0.0001 in
  let q6 = Float.of_int q6 *. 0.0001 in
  let q7 = Float.of_int q7 *. 0.0001 in
  let q8 = Float.of_int q8 *. 0.0001 in
  let q9 = Float.of_int q9 *. 0.0001 in
  let q10 = Float.of_int q10 *. 0.0001 in
  let q11 = Float.of_int q11 *. 0.0001 in
  let q12 = Float.of_int q12 *. 0.0001 in
  let q13 = Float.of_int q13 *. 0.0001 in
  for i = 0 to n - 1 do
    top_dist.(i) <- Float.infinity;
    top_idx.(i) <- 0
  done;
  for centroid = 0 to centroid_k - 1 do
    let d0 = centroids.(centroid) -. q0 in
    let d1 = centroids.(c1_base + centroid) -. q1 in
    let d2 = centroids.(c2_base + centroid) -. q2 in
    let d3 = centroids.(c3_base + centroid) -. q3 in
    let dist4 = (d0 *. d0) +. (d1 *. d1) +. (d2 *. d2) +. (d3 *. d3) in
    if Stdlib.( < ) dist4 top_dist.(n - 1)
    then (
      let d4 = centroids.(c4_base + centroid) -. q4 in
      let d5 = centroids.(c5_base + centroid) -. q5 in
      let d6 = centroids.(c6_base + centroid) -. q6 in
      let d7 = centroids.(c7_base + centroid) -. q7 in
      let dist8 =
        dist4 +. (d4 *. d4) +. (d5 *. d5) +. (d6 *. d6) +. (d7 *. d7)
      in
      if Stdlib.( < ) dist8 top_dist.(n - 1)
      then (
        let d8 = centroids.(c8_base + centroid) -. q8 in
        let d9 = centroids.(c9_base + centroid) -. q9 in
        let d10 = centroids.(c10_base + centroid) -. q10 in
        let d11 = centroids.(c11_base + centroid) -. q11 in
        let d12 = centroids.(c12_base + centroid) -. q12 in
        let d13 = centroids.(c13_base + centroid) -. q13 in
        let dist =
          dist8
          +. (d8 *. d8)
          +. (d9 *. d9)
          +. (d10 *. d10)
          +. (d11 *. d11)
          +. (d12 *. d12)
          +. (d13 *. d13)
        in
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
          top_idx.(pos) <- centroid)))
  done;
  ()
;;

let[@zero_alloc] [@inline always] centroid_ivf_add_candidate_label
  ((best_dist : int array) @ local)
  ((best_label : int array) @ local)
  label
  dist
  =
  if dist < best_dist.(k - 1)
  then (
    if dist < best_dist.(0)
    then (
      best_dist.(4) <- best_dist.(3);
      best_label.(4) <- best_label.(3);
      best_dist.(3) <- best_dist.(2);
      best_label.(3) <- best_label.(2);
      best_dist.(2) <- best_dist.(1);
      best_label.(2) <- best_label.(1);
      best_dist.(1) <- best_dist.(0);
      best_label.(1) <- best_label.(0);
      best_dist.(0) <- dist;
      best_label.(0) <- label)
    else if dist < best_dist.(1)
    then (
      best_dist.(4) <- best_dist.(3);
      best_label.(4) <- best_label.(3);
      best_dist.(3) <- best_dist.(2);
      best_label.(3) <- best_label.(2);
      best_dist.(2) <- best_dist.(1);
      best_label.(2) <- best_label.(1);
      best_dist.(1) <- dist;
      best_label.(1) <- label)
    else if dist < best_dist.(2)
    then (
      best_dist.(4) <- best_dist.(3);
      best_label.(4) <- best_label.(3);
      best_dist.(3) <- best_dist.(2);
      best_label.(3) <- best_label.(2);
      best_dist.(2) <- dist;
      best_label.(2) <- label)
    else if dist < best_dist.(3)
    then (
      best_dist.(4) <- best_dist.(3);
      best_label.(4) <- best_label.(3);
      best_dist.(3) <- dist;
      best_label.(3) <- label)
    else (
      best_dist.(4) <- dist;
      best_label.(4) <- label))
;;

let[@zero_alloc] [@inline always] sq_diff value query =
  let diff = value - query in
  diff * diff
;;

let centroid_ivf_scan_probe
  t
  (best_dist @ local)
  (best_label @ local)
  centroid
  q0
  q1
  q2
  q3
  q4
  q5
  q6
  q7
  q8
  q9
  q10
  q11
  q12
  q13
  =
  let start_block = centroid_ivf_offset t centroid in
  let stop_block = centroid_ivf_offset t (centroid + 1) in
  let blocks_i16 = t.centroid_ivf_blocks_i16 in
  let labels = t.centroid_ivf_labels in
  for block = start_block to stop_block - 1 do
    let block_base = t.centroid_ivf_block_base_i16 + (block * 112) in
    let label_base = t.centroid_ivf_label_base + (block * 8) in
    let base1 = block_base + 8 in
    let base2 = base1 + 8 in
    let base3 = base2 + 8 in
    let base4 = base3 + 8 in
    let base5 = base4 + 8 in
    let base6 = base5 + 8 in
    let base7 = base6 + 8 in
    let base8 = base7 + 8 in
    let base9 = base8 + 8 in
    let base10 = base9 + 8 in
    let base11 = base10 + 8 in
    let base12 = base11 + 8 in
    let base13 = base12 + 8 in
    let dist0 =
      sq_diff (A1.unsafe_get blocks_i16 block_base) q0
      + sq_diff (A1.unsafe_get blocks_i16 base1) q1
      + sq_diff (A1.unsafe_get blocks_i16 base2) q2
      + sq_diff (A1.unsafe_get blocks_i16 base3) q3
      + sq_diff (A1.unsafe_get blocks_i16 base4) q4
      + sq_diff (A1.unsafe_get blocks_i16 base5) q5
      + sq_diff (A1.unsafe_get blocks_i16 base6) q6
      + sq_diff (A1.unsafe_get blocks_i16 base7) q7
      + sq_diff (A1.unsafe_get blocks_i16 base8) q8
      + sq_diff (A1.unsafe_get blocks_i16 base9) q9
      + sq_diff (A1.unsafe_get blocks_i16 base10) q10
      + sq_diff (A1.unsafe_get blocks_i16 base11) q11
      + sq_diff (A1.unsafe_get blocks_i16 base12) q12
      + sq_diff (A1.unsafe_get blocks_i16 base13) q13
    in
    centroid_ivf_add_candidate_label
      best_dist
      best_label
      (A1.unsafe_get labels label_base)
      dist0;
    let dist1 =
      sq_diff (A1.unsafe_get blocks_i16 (block_base + 1)) q0
      + sq_diff (A1.unsafe_get blocks_i16 (base1 + 1)) q1
      + sq_diff (A1.unsafe_get blocks_i16 (base2 + 1)) q2
      + sq_diff (A1.unsafe_get blocks_i16 (base3 + 1)) q3
      + sq_diff (A1.unsafe_get blocks_i16 (base4 + 1)) q4
      + sq_diff (A1.unsafe_get blocks_i16 (base5 + 1)) q5
      + sq_diff (A1.unsafe_get blocks_i16 (base6 + 1)) q6
      + sq_diff (A1.unsafe_get blocks_i16 (base7 + 1)) q7
      + sq_diff (A1.unsafe_get blocks_i16 (base8 + 1)) q8
      + sq_diff (A1.unsafe_get blocks_i16 (base9 + 1)) q9
      + sq_diff (A1.unsafe_get blocks_i16 (base10 + 1)) q10
      + sq_diff (A1.unsafe_get blocks_i16 (base11 + 1)) q11
      + sq_diff (A1.unsafe_get blocks_i16 (base12 + 1)) q12
      + sq_diff (A1.unsafe_get blocks_i16 (base13 + 1)) q13
    in
    centroid_ivf_add_candidate_label
      best_dist
      best_label
      (A1.unsafe_get labels (label_base + 1))
      dist1;
    let dist2 =
      sq_diff (A1.unsafe_get blocks_i16 (block_base + 2)) q0
      + sq_diff (A1.unsafe_get blocks_i16 (base1 + 2)) q1
      + sq_diff (A1.unsafe_get blocks_i16 (base2 + 2)) q2
      + sq_diff (A1.unsafe_get blocks_i16 (base3 + 2)) q3
      + sq_diff (A1.unsafe_get blocks_i16 (base4 + 2)) q4
      + sq_diff (A1.unsafe_get blocks_i16 (base5 + 2)) q5
      + sq_diff (A1.unsafe_get blocks_i16 (base6 + 2)) q6
      + sq_diff (A1.unsafe_get blocks_i16 (base7 + 2)) q7
      + sq_diff (A1.unsafe_get blocks_i16 (base8 + 2)) q8
      + sq_diff (A1.unsafe_get blocks_i16 (base9 + 2)) q9
      + sq_diff (A1.unsafe_get blocks_i16 (base10 + 2)) q10
      + sq_diff (A1.unsafe_get blocks_i16 (base11 + 2)) q11
      + sq_diff (A1.unsafe_get blocks_i16 (base12 + 2)) q12
      + sq_diff (A1.unsafe_get blocks_i16 (base13 + 2)) q13
    in
    centroid_ivf_add_candidate_label
      best_dist
      best_label
      (A1.unsafe_get labels (label_base + 2))
      dist2;
    let dist3 =
      sq_diff (A1.unsafe_get blocks_i16 (block_base + 3)) q0
      + sq_diff (A1.unsafe_get blocks_i16 (base1 + 3)) q1
      + sq_diff (A1.unsafe_get blocks_i16 (base2 + 3)) q2
      + sq_diff (A1.unsafe_get blocks_i16 (base3 + 3)) q3
      + sq_diff (A1.unsafe_get blocks_i16 (base4 + 3)) q4
      + sq_diff (A1.unsafe_get blocks_i16 (base5 + 3)) q5
      + sq_diff (A1.unsafe_get blocks_i16 (base6 + 3)) q6
      + sq_diff (A1.unsafe_get blocks_i16 (base7 + 3)) q7
      + sq_diff (A1.unsafe_get blocks_i16 (base8 + 3)) q8
      + sq_diff (A1.unsafe_get blocks_i16 (base9 + 3)) q9
      + sq_diff (A1.unsafe_get blocks_i16 (base10 + 3)) q10
      + sq_diff (A1.unsafe_get blocks_i16 (base11 + 3)) q11
      + sq_diff (A1.unsafe_get blocks_i16 (base12 + 3)) q12
      + sq_diff (A1.unsafe_get blocks_i16 (base13 + 3)) q13
    in
    centroid_ivf_add_candidate_label
      best_dist
      best_label
      (A1.unsafe_get labels (label_base + 3))
      dist3;
    let dist4 =
      sq_diff (A1.unsafe_get blocks_i16 (block_base + 4)) q0
      + sq_diff (A1.unsafe_get blocks_i16 (base1 + 4)) q1
      + sq_diff (A1.unsafe_get blocks_i16 (base2 + 4)) q2
      + sq_diff (A1.unsafe_get blocks_i16 (base3 + 4)) q3
      + sq_diff (A1.unsafe_get blocks_i16 (base4 + 4)) q4
      + sq_diff (A1.unsafe_get blocks_i16 (base5 + 4)) q5
      + sq_diff (A1.unsafe_get blocks_i16 (base6 + 4)) q6
      + sq_diff (A1.unsafe_get blocks_i16 (base7 + 4)) q7
      + sq_diff (A1.unsafe_get blocks_i16 (base8 + 4)) q8
      + sq_diff (A1.unsafe_get blocks_i16 (base9 + 4)) q9
      + sq_diff (A1.unsafe_get blocks_i16 (base10 + 4)) q10
      + sq_diff (A1.unsafe_get blocks_i16 (base11 + 4)) q11
      + sq_diff (A1.unsafe_get blocks_i16 (base12 + 4)) q12
      + sq_diff (A1.unsafe_get blocks_i16 (base13 + 4)) q13
    in
    centroid_ivf_add_candidate_label
      best_dist
      best_label
      (A1.unsafe_get labels (label_base + 4))
      dist4;
    let dist5 =
      sq_diff (A1.unsafe_get blocks_i16 (block_base + 5)) q0
      + sq_diff (A1.unsafe_get blocks_i16 (base1 + 5)) q1
      + sq_diff (A1.unsafe_get blocks_i16 (base2 + 5)) q2
      + sq_diff (A1.unsafe_get blocks_i16 (base3 + 5)) q3
      + sq_diff (A1.unsafe_get blocks_i16 (base4 + 5)) q4
      + sq_diff (A1.unsafe_get blocks_i16 (base5 + 5)) q5
      + sq_diff (A1.unsafe_get blocks_i16 (base6 + 5)) q6
      + sq_diff (A1.unsafe_get blocks_i16 (base7 + 5)) q7
      + sq_diff (A1.unsafe_get blocks_i16 (base8 + 5)) q8
      + sq_diff (A1.unsafe_get blocks_i16 (base9 + 5)) q9
      + sq_diff (A1.unsafe_get blocks_i16 (base10 + 5)) q10
      + sq_diff (A1.unsafe_get blocks_i16 (base11 + 5)) q11
      + sq_diff (A1.unsafe_get blocks_i16 (base12 + 5)) q12
      + sq_diff (A1.unsafe_get blocks_i16 (base13 + 5)) q13
    in
    centroid_ivf_add_candidate_label
      best_dist
      best_label
      (A1.unsafe_get labels (label_base + 5))
      dist5;
    let dist6 =
      sq_diff (A1.unsafe_get blocks_i16 (block_base + 6)) q0
      + sq_diff (A1.unsafe_get blocks_i16 (base1 + 6)) q1
      + sq_diff (A1.unsafe_get blocks_i16 (base2 + 6)) q2
      + sq_diff (A1.unsafe_get blocks_i16 (base3 + 6)) q3
      + sq_diff (A1.unsafe_get blocks_i16 (base4 + 6)) q4
      + sq_diff (A1.unsafe_get blocks_i16 (base5 + 6)) q5
      + sq_diff (A1.unsafe_get blocks_i16 (base6 + 6)) q6
      + sq_diff (A1.unsafe_get blocks_i16 (base7 + 6)) q7
      + sq_diff (A1.unsafe_get blocks_i16 (base8 + 6)) q8
      + sq_diff (A1.unsafe_get blocks_i16 (base9 + 6)) q9
      + sq_diff (A1.unsafe_get blocks_i16 (base10 + 6)) q10
      + sq_diff (A1.unsafe_get blocks_i16 (base11 + 6)) q11
      + sq_diff (A1.unsafe_get blocks_i16 (base12 + 6)) q12
      + sq_diff (A1.unsafe_get blocks_i16 (base13 + 6)) q13
    in
    centroid_ivf_add_candidate_label
      best_dist
      best_label
      (A1.unsafe_get labels (label_base + 6))
      dist6;
    let dist7 =
      sq_diff (A1.unsafe_get blocks_i16 (block_base + 7)) q0
      + sq_diff (A1.unsafe_get blocks_i16 (base1 + 7)) q1
      + sq_diff (A1.unsafe_get blocks_i16 (base2 + 7)) q2
      + sq_diff (A1.unsafe_get blocks_i16 (base3 + 7)) q3
      + sq_diff (A1.unsafe_get blocks_i16 (base4 + 7)) q4
      + sq_diff (A1.unsafe_get blocks_i16 (base5 + 7)) q5
      + sq_diff (A1.unsafe_get blocks_i16 (base6 + 7)) q6
      + sq_diff (A1.unsafe_get blocks_i16 (base7 + 7)) q7
      + sq_diff (A1.unsafe_get blocks_i16 (base8 + 7)) q8
      + sq_diff (A1.unsafe_get blocks_i16 (base9 + 7)) q9
      + sq_diff (A1.unsafe_get blocks_i16 (base10 + 7)) q10
      + sq_diff (A1.unsafe_get blocks_i16 (base11 + 7)) q11
      + sq_diff (A1.unsafe_get blocks_i16 (base12 + 7)) q12
      + sq_diff (A1.unsafe_get blocks_i16 (base13 + 7)) q13
    in
    centroid_ivf_add_candidate_label
      best_dist
      best_label
      (A1.unsafe_get labels (label_base + 7))
      dist7
  done
;;

let[@zero_alloc] centroid_ivf_scan_probe_cutoff
  t
  (best_dist @ local)
  (best_label @ local)
  centroid
  q0
  q1
  q2
  q3
  q4
  q5
  q6
  q7
  q8
  q9
  q10
  q11
  q12
  q13
  =
  let start_block = centroid_ivf_offset t centroid in
  let stop_block = centroid_ivf_offset t (centroid + 1) in
  let blocks_i16 = t.centroid_ivf_blocks_i16 in
  let labels = t.centroid_ivf_labels in
  for block = start_block to stop_block - 1 do
    let block_base = t.centroid_ivf_block_base_i16 + (block * 112) in
    let label_base = t.centroid_ivf_label_base + (block * 8) in
    let base1 = block_base + 8 in
    let base2 = base1 + 8 in
    let base3 = base2 + 8 in
    let base4 = base3 + 8 in
    let base5 = base4 + 8 in
    let base6 = base5 + 8 in
    let base7 = base6 + 8 in
    let base8 = base7 + 8 in
    let base9 = base8 + 8 in
    let base10 = base9 + 8 in
    let base11 = base10 + 8 in
    let base12 = base11 + 8 in
    let base13 = base12 + 8 in
    let[@inline always] scan_lane lane =
      let dist =
        sq_diff (A1.unsafe_get blocks_i16 (block_base + lane)) q0
        + sq_diff (A1.unsafe_get blocks_i16 (base1 + lane)) q1
        + sq_diff (A1.unsafe_get blocks_i16 (base2 + lane)) q2
        + sq_diff (A1.unsafe_get blocks_i16 (base3 + lane)) q3
        + sq_diff (A1.unsafe_get blocks_i16 (base4 + lane)) q4
        + sq_diff (A1.unsafe_get blocks_i16 (base5 + lane)) q5
      in
      if dist < best_dist.(4)
      then (
        let dist =
          dist
          + sq_diff (A1.unsafe_get blocks_i16 (base6 + lane)) q6
          + sq_diff (A1.unsafe_get blocks_i16 (base7 + lane)) q7
          + sq_diff (A1.unsafe_get blocks_i16 (base8 + lane)) q8
          + sq_diff (A1.unsafe_get blocks_i16 (base9 + lane)) q9
          + sq_diff (A1.unsafe_get blocks_i16 (base10 + lane)) q10
          + sq_diff (A1.unsafe_get blocks_i16 (base11 + lane)) q11
          + sq_diff (A1.unsafe_get blocks_i16 (base12 + lane)) q12
          + sq_diff (A1.unsafe_get blocks_i16 (base13 + lane)) q13
        in
        centroid_ivf_add_candidate_label
          best_dist
          best_label
          (A1.unsafe_get labels (label_base + lane))
          dist)
    in
    scan_lane 0;
    scan_lane 1;
    scan_lane 2;
    scan_lane 3;
    scan_lane 4;
    scan_lane 5;
    scan_lane 6;
    scan_lane 7
  done
;;

let[@zero_alloc] score_frauds_centroid_ivf_nprobe
  t
  (top_dist @ local)
  (top_idx @ local)
  (best_dist @ local)
  (best_label @ local)
  query
  nprobe
  =
  let q0 = query.(0) - 10_000 in
  let q1 = query.(1) - 10_000 in
  let q2 = query.(2) - 10_000 in
  let q3 = query.(3) - 10_000 in
  let q4 = query.(4) - 10_000 in
  let q5 = query.(5) - 10_000 in
  let q6 = query.(6) - 10_000 in
  let q7 = query.(7) - 10_000 in
  let q8 = query.(8) - 10_000 in
  let q9 = query.(9) - 10_000 in
  let q10 = query.(10) - 10_000 in
  let q11 = query.(11) - 10_000 in
  let q12 = query.(12) - 10_000 in
  let q13 = query.(13) - 10_000 in
  centroid_ivf_top_centroids
    t
    top_dist
    top_idx
    q0
    q1
    q2
    q3
    q4
    q5
    q6
    q7
    q8
    q9
    q10
    q11
    q12
    q13
    nprobe;
  reset_best best_dist best_label;
  for i = 0 to nprobe - 1 do
    centroid_ivf_scan_probe_cutoff
      t
      best_dist
      best_label
      top_idx.(i)
      q0
      q1
      q2
      q3
      q4
      q5
      q6
      q7
      q8
      q9
      q10
      q11
      q12
      q13
  done;
  frauds_of_labels best_label
;;

let[@zero_alloc] score_frauds_centroid_ivf_nprobe_presorted
  t
  (top_dist @ local)
  (top_idx @ local)
  (best_dist @ local)
  (best_label @ local)
  query
  top_nprobe
  scan_nprobe
  =
  let q0 = query.(0) - 10_000 in
  let q1 = query.(1) - 10_000 in
  let q2 = query.(2) - 10_000 in
  let q3 = query.(3) - 10_000 in
  let q4 = query.(4) - 10_000 in
  let q5 = query.(5) - 10_000 in
  let q6 = query.(6) - 10_000 in
  let q7 = query.(7) - 10_000 in
  let q8 = query.(8) - 10_000 in
  let q9 = query.(9) - 10_000 in
  let q10 = query.(10) - 10_000 in
  let q11 = query.(11) - 10_000 in
  let q12 = query.(12) - 10_000 in
  let q13 = query.(13) - 10_000 in
  centroid_ivf_top_centroids
    t
    top_dist
    top_idx
    q0
    q1
    q2
    q3
    q4
    q5
    q6
    q7
    q8
    q9
    q10
    q11
    q12
    q13
    top_nprobe;
  reset_best best_dist best_label;
  for i = 0 to scan_nprobe - 1 do
    centroid_ivf_scan_probe_cutoff
      t
      best_dist
      best_label
      top_idx.(i)
      q0
      q1
      q2
      q3
      q4
      q5
      q6
      q7
      q8
      q9
      q10
      q11
      q12
      q13
  done;
  frauds_of_labels best_label
;;

let[@zero_alloc] score_frauds_centroid_ivf_continue_nprobe
  t
  (top_idx @ local)
  (best_dist @ local)
  (best_label @ local)
  query
  scanned_nprobe
  nprobe
  =
  let q0 = query.(0) - 10_000 in
  let q1 = query.(1) - 10_000 in
  let q2 = query.(2) - 10_000 in
  let q3 = query.(3) - 10_000 in
  let q4 = query.(4) - 10_000 in
  let q5 = query.(5) - 10_000 in
  let q6 = query.(6) - 10_000 in
  let q7 = query.(7) - 10_000 in
  let q8 = query.(8) - 10_000 in
  let q9 = query.(9) - 10_000 in
  let q10 = query.(10) - 10_000 in
  let q11 = query.(11) - 10_000 in
  let q12 = query.(12) - 10_000 in
  let q13 = query.(13) - 10_000 in
  for i = scanned_nprobe to nprobe - 1 do
    centroid_ivf_scan_probe_cutoff
      t
      best_dist
      best_label
      top_idx.(i)
      q0
      q1
      q2
      q3
      q4
      q5
      q6
      q7
      q8
      q9
      q10
      q11
      q12
      q13
  done;
  frauds_of_labels best_label
;;

let score_frauds_centroid_ivf_with_scorer_verify t scorer query verify_boundary =
  let top_nprobe =
    if verify_boundary then Int.max t.ivf_verify_nprobe t.ivf_nprobe else t.ivf_nprobe
  in
  let fast =
    score_frauds_centroid_ivf_nprobe_presorted
      t
      scorer.top_dist
      scorer.top_idx
      scorer.best_dist
      scorer.best_label
      query
      top_nprobe
      t.ivf_fast_nprobe
  in
  if fast = 0 || fast = k
  then fast
  else if t.ivf_nprobe > t.ivf_fast_nprobe
  then (
    let frauds =
      score_frauds_centroid_ivf_continue_nprobe
        t
        scorer.top_idx
        scorer.best_dist
        scorer.best_label
        query
        t.ivf_fast_nprobe
        t.ivf_nprobe
    in
    if verify_boundary
       && frauds = 3
       && scorer.best_label.(4) = 1
       && t.ivf_verify_nprobe > t.ivf_nprobe
    then
      score_frauds_centroid_ivf_continue_nprobe
        t
        scorer.top_idx
        scorer.best_dist
        scorer.best_label
        query
        t.ivf_nprobe
        t.ivf_verify_nprobe
    else frauds)
  else
    score_frauds_centroid_ivf_nprobe
      t
      scorer.top_dist
      scorer.top_idx
      scorer.best_dist
      scorer.best_label
      query
      t.ivf_nprobe
;;

let score_frauds_centroid_ivf_with_scorer t scorer query =
  score_frauds_centroid_ivf_with_scorer_verify t scorer query true
;;

let score_frauds_centroid_ivf_verify t query verify_boundary =
  if Int.max t.ivf_verify_nprobe (Int.max t.ivf_nprobe t.ivf_fast_nprobe) <= 40
  then (
    let top_dist =
      stack_
        [| Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
         ; Float.infinity
        |]
    in
    let top_idx =
      stack_
        [| 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
         ; 0
        |]
    in
    let best_dist =
      stack_ [| Int.max_value; Int.max_value; Int.max_value; Int.max_value; Int.max_value |]
    in
    let best_label = stack_ [| 0; 0; 0; 0; 0 |] in
    let q0 = query.(0) - 10_000 in
    let q1 = query.(1) - 10_000 in
    let q2 = query.(2) - 10_000 in
    let q3 = query.(3) - 10_000 in
    let q4 = query.(4) - 10_000 in
    let q5 = query.(5) - 10_000 in
    let q6 = query.(6) - 10_000 in
    let q7 = query.(7) - 10_000 in
    let q8 = query.(8) - 10_000 in
    let q9 = query.(9) - 10_000 in
    let q10 = query.(10) - 10_000 in
    let q11 = query.(11) - 10_000 in
    let q12 = query.(12) - 10_000 in
    let q13 = query.(13) - 10_000 in
    centroid_ivf_top_centroids
      t
      top_dist
      top_idx
      q0
      q1
      q2
      q3
      q4
      q5
      q6
      q7
      q8
      q9
      q10
      q11
      q12
      q13
      (Int.max t.ivf_verify_nprobe (Int.max t.ivf_nprobe t.ivf_fast_nprobe));
    reset_best best_dist best_label;
    for i = 0 to t.ivf_fast_nprobe - 1 do
      centroid_ivf_scan_probe_cutoff
        t
        best_dist
        best_label
        top_idx.(i)
        q0
        q1
        q2
        q3
        q4
        q5
        q6
        q7
        q8
        q9
        q10
        q11
        q12
        q13
    done;
    let fast = frauds_of_labels best_label in
    if fast = 0 || fast = k || t.ivf_fast_nprobe >= t.ivf_nprobe
    then fast
    else (
      for i = t.ivf_fast_nprobe to t.ivf_nprobe - 1 do
        centroid_ivf_scan_probe_cutoff
          t
          best_dist
          best_label
          top_idx.(i)
          q0
          q1
          q2
          q3
          q4
          q5
          q6
          q7
          q8
          q9
          q10
          q11
          q12
          q13
      done;
      let frauds = frauds_of_labels best_label in
      if verify_boundary
         && frauds = 3
         && best_label.(4) = 1
         && t.ivf_verify_nprobe > t.ivf_nprobe
      then (
        for i = t.ivf_nprobe to t.ivf_verify_nprobe - 1 do
          centroid_ivf_scan_probe_cutoff
            t
            best_dist
            best_label
            top_idx.(i)
            q0
            q1
            q2
            q3
            q4
            q5
            q6
            q7
            q8
            q9
            q10
            q11
            q12
            q13
        done;
        (frauds_of_labels best_label) + 0)
      else frauds))
  else score_frauds_centroid_ivf_with_scorer_verify t (create_scorer t) query verify_boundary
;;

let score_frauds_centroid_ivf t query = score_frauds_centroid_ivf_verify t query true

let prewarm t =
  let checksum = ref 0 in
  (match t.vp with
   | None -> ()
   | Some vp ->
     for base = 0 to (reference_rows - 1) * Vectorize.dim do
       checksum := !checksum + A1.unsafe_get vp.vectors base
     done;
     for row = 0 to reference_rows - 1 do
       checksum := !checksum + A1.unsafe_get vp.labels row + i32_bytes_get vp.rows row
     done;
     for node = 0 to vp.node_count - 1 do
       checksum
       := !checksum
          + A1.unsafe_get vp.kinds node
          + i32_bytes_get vp.pivots node
          + i32_bytes_get vp.lefts node
          + i32_bytes_get vp.rights node
          + i32_bytes_get vp.starts node
          + i32_bytes_get vp.counts node
          + Int.of_float vp.radii.(node)
     done);
  let rec touch_index offset =
    if offset < A1.dim t.centroid_ivf_labels
    then (
      checksum := !checksum + A1.unsafe_get t.centroid_ivf_labels offset;
      touch_index (offset + 4096))
  in
  let rec touch_index_i16 offset =
    if offset < A1.dim t.centroid_ivf_blocks_i16
    then (
      checksum := !checksum + A1.unsafe_get t.centroid_ivf_blocks_i16 offset;
      touch_index_i16 (offset + 2048))
  in
  touch_index 0;
  touch_index_i16 0;
  for i = 0 to Array.length t.centroid_ivf_centroids - 1 do
    checksum := !checksum + Int.of_float t.centroid_ivf_centroids.(i)
  done;
  for i = 0 to Array.length t.centroid_ivf_offsets - 1 do
    checksum := !checksum + t.centroid_ivf_offsets.(i)
  done;
  let scorer = create_scorer t in
  let query = Array.create ~len:Vectorize.dim 0 in
  for seed = 0 to 63 do
    for i = 0 to Vectorize.dim - 1 do
      query.(i) <- ((seed * 1543) + (i * 917)) land 0x3fff
    done;
    checksum := !checksum + score_frauds_centroid_ivf_with_scorer t scorer query
  done;
  Sys.opaque_identity !checksum |> ignore;
  ()
;;

let score_frauds_vp t query =
  let vp =
    match t.vp with
    | Some vp -> vp
    | None -> failwith "vp index not loaded"
  in
  let best_dist = Array.create ~len:k Int.max_value in
  let best_label = Array.create ~len:k 0 in
  let best_tau = ref Float.infinity in
  let rec search node =
    if node >= 0 && node < vp.node_count
    then (
      match A1.unsafe_get vp.kinds node with
      | 0 ->
        let start = i32_bytes_get vp.starts node in
        let count = i32_bytes_get vp.counts node in
        for pos = start to start + count - 1 do
          add_candidate vp query best_dist best_label best_tau (i32_bytes_get vp.rows pos)
        done
      | _ ->
        let pivot = i32_bytes_get vp.pivots node in
        let dist_sq = row_distance vp query pivot Int.max_value in
        add_candidate_dist vp best_dist best_label best_tau pivot dist_sq;
        let dist = Float.sqrt (Float.of_int dist_sq) in
        let radius = vp.radii.(node) in
        let left = i32_bytes_get vp.lefts node in
        let right = i32_bytes_get vp.rights node in
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

let score_frauds_with_scorer_verify t _scorer query verify_boundary =
  score_frauds_centroid_ivf_verify t query verify_boundary
;;

let score_frauds_with_scorer t _scorer query =
  score_frauds_centroid_ivf t query
;;
;;

let score_frauds t query = score_frauds_centroid_ivf t query
let score t query = Float.of_int (score_frauds t query) /. Float.of_int k
