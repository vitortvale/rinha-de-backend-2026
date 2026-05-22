type route =
  | Ready
  | Warmup
  | Fraud_score
  | Other

type request =
  { route : route
  ; content_length : int
  ; body_start : int
  }

exception Bad_request
exception Need_more

let[@inline always] ascii_lower_code c =
  let code = Char.code c in
  if code >= Char.code 'A' && code <= Char.code 'Z' then code + 32 else code
;;

let[@inline always] starts_with_bytes b start stop prefix =
  let prefix_len = String.length prefix in
  stop - start >= prefix_len
  &&
  let rec loop i =
    i = prefix_len || (Bytes.unsafe_get b (start + i) = prefix.[i] && loop (i + 1))
  in
  loop 0
;;

let[@inline always] caseless_equal_at b start stop prefix =
  let prefix_len = String.length prefix in
  stop - start >= prefix_len
  &&
  let rec loop i =
    i = prefix_len
    || (ascii_lower_code (Bytes.unsafe_get b (start + i)) = Char.code prefix.[i]
        && loop (i + 1))
  in
  loop 0
;;

let[@inline always] skip_header_ws b i stop =
  let rec loop i =
    if i < stop
       && (Bytes.unsafe_get b i = ' ' || Bytes.unsafe_get b i = '\t')
    then loop (i + 1)
    else i
  in
  loop i
;;

let[@inline always] parse_header_int b i stop =
  let rec loop i acc =
    if i < stop
    then (
      let c = Bytes.unsafe_get b i in
      if c >= '0' && c <= '9'
      then loop (i + 1) ((acc * 10) + Char.code c - Char.code '0')
      else acc)
    else acc
  in
  loop (skip_header_ws b i stop) 0
;;

let[@inline always] next_line b i stop =
  let rec loop j =
    if j >= stop
    then stop, stop
    else if Bytes.unsafe_get b j = '\r'
            && j + 1 < stop
            && Bytes.unsafe_get b (j + 1) = '\n'
    then j, j + 2
    else loop (j + 1)
  in
  loop i
;;

let[@inline always] route_of_request_line b start stop =
  if starts_with_bytes b start stop "POST /fraud-score "
  then Fraud_score
  else if starts_with_bytes b start stop "GET /warmup "
  then Warmup
  else if starts_with_bytes b start stop "GET /ready "
  then Ready
  else Other
;;

let scan_request b len =
  let line_stop, next = next_line b 0 len in
  if line_stop = len then raise Need_more;
  let route = route_of_request_line b 0 line_stop in
  let rec loop i content_length =
    if i + 1 >= len
    then raise Need_more
    else if Bytes.unsafe_get b i = '\r' && Bytes.unsafe_get b (i + 1) = '\n'
    then { route; content_length; body_start = i + 2 }
    else if Bytes.unsafe_get b i = '\n'
    then { route; content_length; body_start = i + 1 }
    else (
      let line_stop, next = next_line b i len in
      if line_stop = len
      then raise Need_more
      else if caseless_equal_at b i line_stop "content-length:"
      then loop next (parse_header_int b (i + 15) line_stop)
      else loop next content_length)
  in
  loop next 0
;;

let response status reason body =
  Printf.sprintf
    "HTTP/1.1 %d %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
    status
    reason
    (String.length body)
    body
;;

let response_json score =
  let approved = score < 0.6 in
  Printf.sprintf {|{"approved":%s,"fraud_score":%.1f}|} (string_of_bool approved) score
;;

let ready_response = response 200 "OK" "OK"
let bad_request_response = response 400 "Bad Request" "bad request"
let not_found_response = response 404 "Not Found" "not found"

let fraud_responses =
  Array.init (Index.k + 1) (fun frauds ->
    let score = float_of_int frauds /. float_of_int Index.k in
    response 200 "OK" (response_json score))
;;
