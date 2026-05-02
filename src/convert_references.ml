open Core

let dim = 14
let scale = 10000.

let quantize value =
  Float.iround_nearest_exn ((value +. 1.) *. scale)
  |> Int.clamp_exn ~min:0 ~max:65535

let write_u16_le output value =
  Out_channel.output_char output (Char.of_int_exn (value land 0xff));
  Out_channel.output_char output (Char.of_int_exn ((value lsr 8) land 0xff))

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
          if not (String.is_prefix (String.drop_prefix input !pos) ~prefix:label_marker)
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
      let count = loop 0 0 in
      eprintf "converted %d references\n%!" count))

let command =
  Command.basic
    ~summary:"Convert references JSON into packed u16 vectors and u8 labels"
    (let%map_open.Command input_path = anon ("references-json" %: string)
     and vectors_path = anon ("references-u16" %: string)
     and labels_path = anon ("labels-u8" %: string) in
     fun () -> convert input_path vectors_path labels_path)

let () = Command_unix.run command
