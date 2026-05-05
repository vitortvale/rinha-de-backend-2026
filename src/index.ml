module BA = Bigarray
module A1 = Bigarray.Array1
module Std_unix = Unix

open Core

type t =
  { vectors : (int, BA.int16_unsigned_elt, BA.c_layout) A1.t
  ; labels : (int, BA.int8_unsigned_elt, BA.c_layout) A1.t
  ; rows : (int32, BA.int32_elt, BA.c_layout) A1.t
  ; kinds : (int, BA.int8_unsigned_elt, BA.c_layout) A1.t
  ; pivots : (int32, BA.int32_elt, BA.c_layout) A1.t
  ; radii : float array
  ; lefts : (int32, BA.int32_elt, BA.c_layout) A1.t
  ; rights : (int32, BA.int32_elt, BA.c_layout) A1.t
  ; starts : (int32, BA.int32_elt, BA.c_layout) A1.t
  ; counts : (int32, BA.int32_elt, BA.c_layout) A1.t
  ; node_count : int
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

let map_file path kind len =
  let fd = Std_unix.openfile path [ Std_unix.O_RDONLY ] 0 in
  let mapped = Std_unix.map_file fd kind BA.c_layout false [| len |] in
  Std_unix.close fd;
  BA.array1_of_genarray mapped

let load_u16_anon path len =
  let src = map_file path BA.int16_unsigned len in
  let dst = A1.create BA.int16_unsigned BA.c_layout len in
  for i = 0 to len - 1 do
    A1.unsafe_set dst i (A1.unsafe_get src i)
  done;
  dst

let load_u8_anon path len =
  let src = map_file path BA.int8_unsigned len in
  let dst = A1.create BA.int8_unsigned BA.c_layout len in
  for i = 0 to len - 1 do
    A1.unsafe_set dst i (A1.unsafe_get src i)
  done;
  dst

let map_i32 path len = map_file path BA.int32 len
let i32_get a i = A1.unsafe_get a i |> Stdlib.Int32.to_int
let i64_get a i = A1.unsafe_get a i |> Int64.to_int_exn
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
  Gc.full_major ();
  { vectors = load_u16_anon vectors_path (reference_rows * Vectorize.dim)
  ; labels = load_u8_anon labels_path reference_rows
  ; rows = map_i32 rows_path reference_rows
  ; kinds = map_file kind_path BA.int8_unsigned node_count
  ; pivots = map_i32 pivot_path node_count
  ; radii
  ; lefts = map_i32 left_path node_count
  ; rights = map_i32 right_path node_count
  ; starts = map_i32 start_path node_count
  ; counts = map_i32 count_path node_count
  ; node_count
  }

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

let add_candidate t query best_dist best_label best_tau row =
  let dist = row_distance t query row best_dist.(k - 1) in
  add_candidate_dist t best_dist best_label best_tau row dist

let prewarm t =
  let checksum = ref 0 in
  for base = 0 to (reference_rows - 1) * Vectorize.dim do
    checksum := !checksum + A1.unsafe_get t.vectors base
  done;
  for row = 0 to reference_rows - 1 do
    checksum := !checksum + A1.unsafe_get t.labels row + i32_get t.rows row
  done;
  for node = 0 to t.node_count - 1 do
    checksum
    := !checksum
       + A1.unsafe_get t.kinds node
       + i32_get t.pivots node
       + i32_get t.lefts node
       + i32_get t.rights node
       + i32_get t.starts node
       + i32_get t.counts node
       + Int.of_float t.radii.(node)
  done;
  Sys.opaque_identity !checksum |> ignore;
  ()

let score_frauds t query =
  let best_dist = Array.create ~len:k Int.max_value in
  let best_label = Array.create ~len:k 0 in
  let best_tau = ref Float.infinity in
  let rec search node =
    if node >= 0 && node < t.node_count
    then (
      match A1.unsafe_get t.kinds node with
      | 0 ->
        let start = i32_get t.starts node in
        let count = i32_get t.counts node in
        for pos = start to start + count - 1 do
          add_candidate t query best_dist best_label best_tau (i32_get t.rows pos)
        done
      | _ ->
        let pivot = i32_get t.pivots node in
        let dist_sq = row_distance t query pivot Int.max_value in
        add_candidate_dist t best_dist best_label best_tau pivot dist_sq;
        let dist = Float.sqrt (Float.of_int dist_sq) in
        let radius = t.radii.(node) in
        let left = i32_get t.lefts node in
        let right = i32_get t.rights node in
        if Float.(dist < radius)
        then (
          search left;
          if Float.(dist + !best_tau >= radius) then search right)
        else (
          search right;
          if Float.(dist - !best_tau <= radius) then search left))
  in
  search 0;
  let frauds =
    best_label.(0) + best_label.(1) + best_label.(2) + best_label.(3) + best_label.(4)
  in
  frauds

let score t query = Float.of_int (score_frauds t query) /. Float.of_int k
