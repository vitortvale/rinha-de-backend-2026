open Core

module BA = Bigarray
module A1 = Bigarray.Array1
module Std_unix = Caml_unix

type t =
  { vectors : (int, BA.int16_unsigned_elt, BA.c_layout) A1.t
  ; labels : (int, BA.int8_unsigned_elt, BA.c_layout) A1.t
  ; rows : int
  }

let map_file path kind len =
  let fd = Std_unix.openfile path [ Std_unix.O_RDONLY ] 0 in
  let mapped = Std_unix.map_file fd kind BA.c_layout false [| len |] in
  Std_unix.close fd;
  BA.array1_of_genarray mapped

let load data_dir =
  let vectors_path = Filename.concat data_dir "references.u16" in
  let labels_path = Filename.concat data_dir "labels.u8" in
  let vector_bytes = (Std_unix.stat vectors_path).st_size in
  let rows = vector_bytes / (Vectorize.dim * 2) in
  { vectors = map_file vectors_path BA.int16_unsigned (rows * Vectorize.dim)
  ; labels = map_file labels_path BA.int8_unsigned rows
  ; rows
  }

let rec distance_until vectors query base limit dim acc =
  if dim = Vectorize.dim || acc >= limit
  then acc
  else (
    let diff = query.(dim) - A1.unsafe_get vectors (base + dim) in
    distance_until vectors query base limit (dim + 1) (acc + (diff * diff)))

let score t query =
  let best_dist = Array.create ~len:5 Int.max_value in
  let best_label = Array.create ~len:5 0 in
  for row = 0 to t.rows - 1 do
    let base = row * Vectorize.dim in
    let dist = distance_until t.vectors query base best_dist.(4) 0 0 in
    if dist < best_dist.(4)
    then (
      let rec insertion_pos pos =
        if pos > 0 && dist < best_dist.(pos - 1)
        then (
          best_dist.(pos) <- best_dist.(pos - 1);
          best_label.(pos) <- best_label.(pos - 1);
          insertion_pos (pos - 1))
        else pos
      in
      let pos = insertion_pos 4 in
      best_dist.(pos) <- dist;
      best_label.(pos) <- A1.unsafe_get t.labels row)
  done;
  let frauds =
    best_label.(0) + best_label.(1) + best_label.(2) + best_label.(3) + best_label.(4)
  in
  Float.of_int frauds /. 5.
