module BA = Bigarray
module A1 = Bigarray.Array1
module Std_unix = Unix

open Core

let dim = 14
let scale = 10000.
let leaf_size = 64

let quantize value =
  Float.iround_nearest_exn ((value +. 1.) *. scale)
  |> Int.clamp_exn ~min:0 ~max:65535

let write_u16_le output value =
  Out_channel.output_char output (Char.of_int_exn (value land 0xff));
  Out_channel.output_char output (Char.of_int_exn ((value lsr 8) land 0xff))

let write_i32_le output value =
  let value = Int32.to_int_exn value in
  Out_channel.output_char output (Char.of_int_exn (value land 0xff));
  Out_channel.output_char output (Char.of_int_exn ((value lsr 8) land 0xff));
  Out_channel.output_char output (Char.of_int_exn ((value lsr 16) land 0xff));
  Out_channel.output_char output (Char.of_int_exn ((value lsr 24) land 0xff))

let write_i64_le output value =
  let value = Int64.to_int_exn value in
  for shift = 0 to 7 do
    Out_channel.output_char output (Char.of_int_exn ((value lsr (shift * 8)) land 0xff))
  done

let has_prefix_at s pos prefix =
  let prefix_len = String.length prefix in
  pos + prefix_len <= String.length s
  &&
  let rec loop i =
    i = prefix_len || (Char.equal s.[pos + i] prefix.[i] && loop (i + 1))
  in
  loop 0

let parse_float_at s i =
  let len = String.length s in
  let rec loop j =
    if j < len
       && (Char.is_digit s.[j]
           || Char.equal s.[j] '.'
           || Char.equal s.[j] '-'
           || Char.equal s.[j] '+'
           || Char.equal s.[j] 'e'
           || Char.equal s.[j] 'E')
    then loop (j + 1)
    else j
  in
  let j = loop i in
  Float.of_string (String.sub s ~pos:i ~len:(j - i)), j

let map_vectors path rows =
  let fd = Std_unix.openfile path [ Std_unix.O_RDONLY ] 0 in
  let mapped = Std_unix.map_file fd BA.int16_unsigned BA.c_layout false [| rows * dim |] in
  Std_unix.close fd;
  BA.array1_of_genarray mapped

let distance vectors a b =
  let base_a = a * dim in
  let base_b = b * dim in
  let acc = ref 0 in
  for i = 0 to dim - 1 do
    let diff = A1.unsafe_get vectors (base_a + i) - A1.unsafe_get vectors (base_b + i) in
    acc := !acc + (diff * diff)
  done;
  !acc

let swap a i j =
  let tmp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- tmp

let partition_by_distance rows distances left right pivot_index =
  let pivot_distance = distances.(pivot_index) in
  swap rows pivot_index right;
  swap distances pivot_index right;
  let store = ref left in
  for i = left to right - 1 do
    if distances.(i) < pivot_distance
    then (
      swap rows !store i;
      swap distances !store i;
      incr store)
  done;
  swap rows !store right;
  swap distances !store right;
  !store

let rec select_distance vectors rows distances left right k =
  if left < right
  then (
    let pivot_index = left + ((right - left) / 2) in
    let pivot_index = partition_by_distance rows distances left right pivot_index in
    if k < pivot_index
    then select_distance vectors rows distances left (pivot_index - 1) k
    else if k > pivot_index
    then select_distance vectors rows distances (pivot_index + 1) right k)

let build_vp_tree vectors rows_count rows_path kind_path pivot_path radius_path left_path right_path
  start_path count_path =
  let max_nodes = ((rows_count / leaf_size) * 3) + 16 in
  let kinds = Array.create ~len:max_nodes 0 in
  let pivots = Array.create ~len:max_nodes 0 in
  let radii = Array.create ~len:max_nodes 0 in
  let lefts = Array.create ~len:max_nodes (-1) in
  let rights = Array.create ~len:max_nodes (-1) in
  let starts = Array.create ~len:max_nodes 0 in
  let counts = Array.create ~len:max_nodes 0 in
  let rows = Array.init rows_count ~f:Fn.id in
  let distances = Array.create ~len:rows_count 0 in
  let next_node = ref 0 in
  let new_node () =
    let node = !next_node in
    incr next_node;
    if !next_node > max_nodes then failwith "vp tree node capacity exceeded";
    node
  in
  let rec build lo hi =
    let node = new_node () in
    let n = hi - lo in
    if n <= leaf_size
    then (
      kinds.(node) <- 0;
      starts.(node) <- lo;
      counts.(node) <- n;
      node)
    else (
      let pivot = rows.(lo) in
      let dist_lo = lo + 1 in
      let dist_hi = hi - 1 in
      for i = dist_lo to dist_hi do
        distances.(i) <- distance vectors pivot rows.(i)
      done;
      let mid = dist_lo + ((dist_hi - dist_lo) / 2) in
      select_distance vectors rows distances dist_lo dist_hi mid;
      kinds.(node) <- 1;
      pivots.(node) <- pivot;
      radii.(node) <- distances.(mid);
      lefts.(node) <- build dist_lo (mid + 1);
      if mid + 1 < hi then rights.(node) <- build (mid + 1) hi;
      node)
  in
  ignore (build 0 rows_count);
  Out_channel.with_file rows_path ~binary:true ~f:(fun output ->
    for i = 0 to rows_count - 1 do
      write_i32_le output (Int32.of_int_exn rows.(i))
    done);
  let write_int_array path arr =
    Out_channel.with_file path ~binary:true ~f:(fun output ->
      for i = 0 to !next_node - 1 do
        write_i32_le output (Int32.of_int_exn arr.(i))
      done)
  in
  Out_channel.with_file kind_path ~binary:true ~f:(fun output ->
    for i = 0 to !next_node - 1 do
      Out_channel.output_char output (Char.of_int_exn kinds.(i))
    done);
  write_int_array pivot_path pivots;
  Out_channel.with_file radius_path ~binary:true ~f:(fun output ->
    for i = 0 to !next_node - 1 do
      write_i64_le output (Int64.of_int radii.(i))
    done);
  write_int_array left_path lefts;
  write_int_array right_path rights;
  write_int_array start_path starts;
  write_int_array count_path counts;
  eprintf "built vp tree with %d nodes\n%!" !next_node

let convert input_path vectors_path labels_path vp_dir =
  let input = In_channel.read_all input_path in
  let marker = "{\"vector\":[" in
  let label_marker = "],\"label\":\"" in
  let len = String.length input in
  let count =
    Out_channel.with_file vectors_path ~binary:true ~f:(fun vectors ->
      Out_channel.with_file labels_path ~binary:true ~f:(fun labels ->
        let rec loop offset count =
          match String.substr_index input ~pos:offset ~pattern:marker with
          | None -> count
          | Some start ->
            let pos = ref (start + String.length marker) in
            for dim_index = 0 to dim - 1 do
              let value, next = parse_float_at input !pos in
              write_u16_le vectors (quantize value);
              pos := next;
              if dim_index < dim - 1
              then (
                if !pos >= len || not (Char.equal input.[!pos] ',')
                then failwithf "bad vector separator at row %d dim %d" count dim_index ();
                incr pos)
            done;
            if not (has_prefix_at input !pos label_marker)
            then failwithf "bad label marker at row %d" count ();
            pos := !pos + String.length label_marker;
            let fraud =
              !pos + 5 <= len && String.equal (String.sub input ~pos:!pos ~len:5) "fraud"
            in
            Out_channel.output_char labels (if fraud then '\001' else '\000');
            let next =
              match String.index_from input !pos '}' with
              | Some end_record -> end_record + 1
              | None -> len
            in
            loop next (count + 1)
        in
        loop 0 0))
  in
  eprintf "converted %d references\n%!" count;
  Option.iter vp_dir ~f:(fun vp_dir ->
    let vectors = map_vectors vectors_path count in
    build_vp_tree
      vectors
      count
      (Filename.concat vp_dir "vp_rows.i32")
      (Filename.concat vp_dir "vp_kind.u8")
      (Filename.concat vp_dir "vp_pivot.i32")
      (Filename.concat vp_dir "vp_radius.i64")
      (Filename.concat vp_dir "vp_left.i32")
      (Filename.concat vp_dir "vp_right.i32")
      (Filename.concat vp_dir "vp_start.i32")
      (Filename.concat vp_dir "vp_count.i32"))

let () =
  match Sys.get_argv () |> Array.to_list with
  | [ _; input_path; vectors_path; labels_path ] ->
    convert input_path vectors_path labels_path None
  | [ _; input_path; vectors_path; labels_path; vp_dir ] ->
    convert input_path vectors_path labels_path (Some vp_dir)
  | argv ->
    eprintf
      "usage: %s references.json references.u16 labels.u8 [vp-output-dir]\n%!"
      (List.hd argv |> Option.value ~default:"convert_references");
    exit 2
