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
  ; radii : (int64, BA.int64_elt, BA.c_layout) A1.t
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

let map_i32 path len = map_file path BA.int32 len
let i32_get a i = A1.unsafe_get a i |> Int32.to_int_exn
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
  { vectors = map_file vectors_path BA.int16_unsigned (reference_rows * Vectorize.dim)
  ; labels = map_file labels_path BA.int8_unsigned reference_rows
  ; rows = map_i32 rows_path reference_rows
  ; kinds = map_file kind_path BA.int8_unsigned node_count
  ; pivots = map_i32 pivot_path node_count
  ; radii = map_file radius_path BA.int64 node_count
  ; lefts = map_i32 left_path node_count
  ; rights = map_i32 right_path node_count
  ; starts = map_i32 start_path node_count
  ; counts = map_i32 count_path node_count
  ; node_count
  }

let distance_until vectors query base limit dim acc =
  let rec loop dim acc =
    if dim = Vectorize.dim || acc >= limit
    then acc
    else (
      let diff = query.(dim) - A1.unsafe_get vectors (base + dim) in
      loop (dim + 1) (acc + (diff * diff)))
  in
  loop dim acc

let row_distance t query row limit =
  distance_until t.vectors query (row * Vectorize.dim) limit 0 0

let add_candidate t query best_dist best_label row =
  let dist = row_distance t query row best_dist.(k - 1) in
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
    best_label.(pos) <- A1.unsafe_get t.labels row)

let tau best_dist =
  if best_dist.(k - 1) = Int.max_value
  then Float.infinity
  else Float.sqrt (Float.of_int best_dist.(k - 1))

let score t query =
  let best_dist = Array.create ~len:k Int.max_value in
  let best_label = Array.create ~len:k 0 in
  let rec search node =
    if node >= 0 && node < t.node_count
    then (
      match A1.unsafe_get t.kinds node with
      | 0 ->
        let start = i32_get t.starts node in
        let count = i32_get t.counts node in
        for pos = start to start + count - 1 do
          add_candidate t query best_dist best_label (i32_get t.rows pos)
        done
      | _ ->
        let pivot = i32_get t.pivots node in
        let dist_sq = row_distance t query pivot Int.max_value in
        add_candidate t query best_dist best_label pivot;
        let dist = Float.sqrt (Float.of_int dist_sq) in
        let radius = Float.sqrt (Float.of_int (i64_get t.radii node)) in
        let left = i32_get t.lefts node in
        let right = i32_get t.rights node in
        if Float.(dist < radius)
        then (
          search left;
          if Float.(dist + tau best_dist >= radius) then search right)
        else (
          search right;
          if Float.(dist - tau best_dist <= radius) then search left))
  in
  search 0;
  let frauds =
    best_label.(0) + best_label.(1) + best_label.(2) + best_label.(3) + best_label.(4)
  in
  Float.of_int frauds /. Float.of_int k
