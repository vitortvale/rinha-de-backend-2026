open Core
open Rinha_lib

let approx a b = Float.(abs (a -. b) < 0.0002)

let quantize x =
  Int.clamp_exn (Float.iround_nearest_exn ((x +. 1.) *. 10000.)) ~min:0 ~max:65535

let expected_quantized config request =
  Runtime_json.parse request |> Vectorize.to_float_array config |> Array.map ~f:quantize

let assert_quantized_matches_float_path config request =
  assert (
    Array.equal
      Int.equal
      (Vectorize.to_quantized config request)
      (expected_quantized config request))

let is_space = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false

let skip_ws s i =
  let len = String.length s in
  let rec loop i = if i < len && is_space s.[i] then loop (i + 1) else i in
  loop i

let object_end s start =
  let len = String.length s in
  let rec loop i depth in_string escaped =
    if i >= len
    then failwith "unterminated json object"
    else (
      let c = s.[i] in
      if in_string
      then (
        let escaped' = (not escaped) && Char.equal c '\\' in
        let in_string' = if (not escaped) && Char.equal c '"' then false else true in
        loop (i + 1) depth in_string' escaped')
      else (
        match c with
        | '"' -> loop (i + 1) depth true false
        | '{' -> loop (i + 1) (depth + 1) false false
        | '}' ->
          let depth = depth - 1 in
          if depth = 0 then i else loop (i + 1) depth false false
        | _ -> loop (i + 1) depth false false))
  in
  loop start 0 false false

let value_object_start s key_pos =
  let len = String.length s in
  let rec colon i =
    if i >= len
    then failwith "missing colon"
    else if Char.equal s.[i] ':'
    then i + 1
    else colon (i + 1)
  in
  let i = skip_ws s (colon key_pos) in
  if i >= len || not (Char.equal s.[i] '{') then failwith "expected request object";
  i

let iter_requests path ~f =
  let input = In_channel.read_all path in
  let marker = "\"request\"" in
  let rec loop offset =
    match String.substr_index input ~pos:offset ~pattern:marker with
    | None -> ()
    | Some marker_pos ->
      let start = value_object_start input (marker_pos + String.length marker) in
      let stop = object_end input start in
      f (String.sub input ~pos:start ~len:(stop - start + 1));
      loop (stop + 1)
  in
  loop 0

let () =
  let config = Config.load "../resources" in
  let request =
    {|{"id":"tx-1329056812","transaction":{"amount":41.12,"installments":2,"requested_at":"2026-03-11T18:45:53Z"},"customer":{"avg_amount":82.24,"tx_count_24h":3,"known_merchants":["MERC-003","MERC-016"]},"merchant":{"id":"MERC-016","mcc":"5411","avg_amount":60.25},"terminal":{"is_online":false,"card_present":true,"km_from_home":29.23},"last_transaction":null}|}
  in
  let tx = Runtime_json.parse request in
  let v = Vectorize.to_float_array config tx in
  assert (Array.length v = 14);
  assert (approx v.(0) 0.0041);
  assert (approx v.(1) 0.1667);
  assert (approx v.(2) 0.05);
  assert (approx v.(3) 0.7826);
  assert (approx v.(4) 0.3333);
  assert (Float.equal v.(5) (-1.));
  assert (Float.equal v.(6) (-1.));
  assert (approx v.(7) 0.0292);
  assert (approx v.(8) 0.15);
  assert (Float.equal v.(9) 0.);
  assert (Float.equal v.(10) 1.);
  assert (Float.equal v.(11) 0.);
  assert (approx v.(12) 0.15);
  assert (approx v.(13) 0.006);
  assert_quantized_matches_float_path config request;
  let test_data = "/tmp/rinha-2026-specs/test/test-data.json" in
  if Sys_unix.file_exists_exn test_data
  then iter_requests test_data ~f:(assert_quantized_matches_float_path config)
