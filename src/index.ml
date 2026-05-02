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

let score t query =
  let best_dist = Array.create ~len:5 Int.max_value in
  let best_label = Array.create ~len:5 0 in
  for row = 0 to t.rows - 1 do
    let base = row * Vectorize.dim in
    let dist = ref 0 in
    for dim = 0 to Vectorize.dim - 1 do
      let diff = query.(dim) - A1.unsafe_get t.vectors (base + dim) in
      dist := !dist + (diff * diff)
    done;
    if !dist < best_dist.(4)
    then (
      let pos = ref 4 in
      while !pos > 0 && !dist < best_dist.(!pos - 1) do
        best_dist.(!pos) <- best_dist.(!pos - 1);
        best_label.(!pos) <- best_label.(!pos - 1);
        decr pos
      done;
      best_dist.(!pos) <- !dist;
      best_label.(!pos) <- A1.unsafe_get t.labels row)
  done;
  let frauds = Array.fold best_label ~init:0 ~f:( + ) in
  Float.of_int frauds /. 5.
