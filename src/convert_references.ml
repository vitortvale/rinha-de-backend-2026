module BA = Bigarray
module A1 = Bigarray.Array1
module Std_unix = Unix

open Core

let dim = 14
let scale = 10000.

let quantize value =
  Float.iround_nearest_exn ((value +. 1.) *. scale)
  |> Int.clamp_exn ~min:0 ~max:65535

let write_u16_le output value =
  Out_channel.output_char output (Char.of_int_exn (value land 0xff));
  Out_channel.output_char output (Char.of_int_exn ((value lsr 8) land 0xff))

let u32_le_bytes bytes offset =
  let b0 = Char.to_int (Bytes.get bytes offset) in
  let b1 = Char.to_int (Bytes.get bytes (offset + 1)) in
  let b2 = Char.to_int (Bytes.get bytes (offset + 2)) in
  let b3 = Char.to_int (Bytes.get bytes (offset + 3)) in
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

let really_read_at fd offset len =
  ignore (Std_unix.lseek fd offset Std_unix.SEEK_SET : int);
  let bytes = Bytes.create len in
  let rec loop pos =
    if pos < len
    then (
      let read = Std_unix.read fd bytes pos (len - pos) in
      if read = 0 then failwith "unexpected EOF while reading centroid IVF index";
      loop (pos + read))
  in
  loop 0;
  bytes

let write_all fd bytes pos len =
  let rec loop pos remaining =
    if remaining > 0
    then (
      let written = Std_unix.write fd bytes pos remaining in
      if written = 0 then failwith "short write while splitting centroid IVF index";
      loop (pos + written) (remaining - written))
  in
  loop pos len

let copy_exact input_fd output_fd bytes_to_copy =
  let buffer = Bytes.create (1024 * 1024) in
  let rec loop remaining =
    if remaining > 0
    then (
      let chunk = Int.min remaining (Bytes.length buffer) in
      let read = Std_unix.read input_fd buffer 0 chunk in
      if read = 0 then failwith "unexpected EOF while splitting centroid IVF index";
      write_all output_fd buffer 0 read;
      loop (remaining - read))
  in
  loop bytes_to_copy

let copy_range input_fd output_path ~offset ~len =
  ignore (Std_unix.lseek input_fd offset Std_unix.SEEK_SET : int);
  let output_fd =
    Std_unix.openfile
      output_path
      [ Std_unix.O_WRONLY; Std_unix.O_CREAT; Std_unix.O_TRUNC ]
      0o644
  in
  Exn.protect
    ~f:(fun () -> copy_exact input_fd output_fd len)
    ~finally:(fun () -> Std_unix.close output_fd)

let split_centroid_ivf input_path meta_path labels_path blocks_path =
  let input_fd = Std_unix.openfile input_path [ Std_unix.O_RDONLY ] 0 in
  Exn.protect
    ~f:(fun () ->
      let header = really_read_at input_fd 0 16 in
      if not (Char.equal (Bytes.get header 0) 'I') then failwith "bad centroid IVF index";
      let rows = u32_le_bytes header 4 in
      let centroid_k = u32_le_bytes header 8 in
      let centroid_dim = u32_le_bytes header 12 in
      if rows <> 3_000_000 || centroid_k <> 4096 || centroid_dim <> dim
      then failwith "bad centroid IVF metadata";
      let offsets_base = 16 + (centroid_dim * centroid_k * 4) in
      let total_blocks =
        u32_le_bytes (really_read_at input_fd (offsets_base + (centroid_k * 4)) 4) 0
      in
      let labels_base = offsets_base + ((centroid_k + 1) * 4) in
      let labels_len = total_blocks * 8 in
      let blocks_base = labels_base + labels_len in
      let blocks_len = total_blocks * centroid_dim * 8 * 2 in
      let input_len = (Std_unix.stat input_path).st_size in
      if input_len <> blocks_base + blocks_len
      then
        failwithf
          "bad centroid IVF size: got %d bytes, expected %d"
          input_len
          (blocks_base + blocks_len)
          ();
      copy_range input_fd meta_path ~offset:0 ~len:labels_base;
      copy_range input_fd labels_path ~offset:labels_base ~len:labels_len;
      copy_range input_fd blocks_path ~offset:blocks_base ~len:blocks_len;
      eprintf
        "split centroid IVF: meta=%d labels=%d blocks=%d total_blocks=%d\n%!"
        labels_base
        labels_len
        blocks_len
        total_blocks)
    ~finally:(fun () -> Std_unix.close input_fd)

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

let convert input_path vectors_path labels_path =
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
  eprintf "converted %d references\n%!" count

let () =
  match Sys.get_argv () |> Array.to_list with
  | [ _; "split-ivf"; input_path; meta_path; labels_path; blocks_path ] ->
    split_centroid_ivf input_path meta_path labels_path blocks_path
  | [ _; input_path; vectors_path; labels_path ] ->
    convert input_path vectors_path labels_path
  | argv ->
    eprintf
      "usage: %s references.json references.u16 labels.u8\n\
       or: %s split-ivf centroid_ivf_index.bin meta.bin labels.u8 blocks.i16\n%!"
      (List.hd argv |> Option.value ~default:"convert_references")
      (List.hd argv |> Option.value ~default:"convert_references");
    exit 2
