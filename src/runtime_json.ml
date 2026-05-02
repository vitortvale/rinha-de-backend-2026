open Core

type last_transaction =
  | Missing
  | Present of
      { timestamp : string
      ; km_from_current : float
      }

type transaction =
  { amount : float
  ; installments : int
  ; requested_at : string
  ; customer_avg_amount : float
  ; tx_count_24h : int
  ; merchant_id : string
  ; merchant_mcc : string
  ; merchant_avg_amount : float
  ; is_online : bool
  ; card_present : bool
  ; km_from_home : float
  ; known_merchant : bool
  ; last_transaction : last_transaction
  }

let is_space = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false

let skip_ws s i =
  let len = String.length s in
  let rec loop i = if i < len && is_space s.[i] then loop (i + 1) else i in
  loop i

let find_key ?(from = 0) s key =
  let pattern = "\"" ^ key ^ "\"" in
  match String.substr_index ~pos:from s ~pattern with
  | Some i -> i + String.length pattern
  | None -> failwith ("missing json key: " ^ key)

let value_start ?(from = 0) s key =
  let len = String.length s in
  let rec colon i = if i >= len then failwith "missing colon" else if Char.equal s.[i] ':' then i + 1 else colon (i + 1) in
  skip_ws s (colon (find_key ~from s key))

let parse_number_at s i =
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

let number ?from s key = parse_number_at s (value_start ?from s key) |> fst
let int ?from s key = Float.to_int (number ?from s key)

let string ?from s key =
  let i = value_start ?from s key in
  if not (Char.equal s.[i] '"') then failwith ("expected string: " ^ key);
  let rec loop j =
    if j >= String.length s then failwith "unterminated string";
    if Char.equal s.[j] '"' && not (Char.equal s.[j - 1] '\\') then j else loop (j + 1)
  in
  let j = loop (i + 1) in
  String.sub s ~pos:(i + 1) ~len:(j - i - 1)

let bool ?from s key =
  let i = value_start ?from s key in
  String.is_prefix (String.drop_prefix s i) ~prefix:"true"

let object_bounds ?(from = 0) s key =
  let start = value_start ~from s key in
  if not (Char.equal s.[start] '{') then failwith ("expected object: " ^ key);
  let rec loop i depth in_string escaped =
    if i >= String.length s then failwith "unterminated object";
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
        if depth = 0 then start, i else loop (i + 1) depth false false
      | _ -> loop (i + 1) depth false false)
  in
  loop start 0 false false

let array_contains_string ?from s key needle =
  let i = value_start ?from s key in
  if not (Char.equal s.[i] '[') then failwith ("expected array: " ^ key);
  let rec loop i =
    if i >= String.length s then false
    else (
      match s.[i] with
      | ']' -> false
      | '"' ->
        let j = Option.value_exn (String.index_from s (i + 1) '"') in
        if String.equal needle (String.sub s ~pos:(i + 1) ~len:(j - i - 1)) then true else loop (j + 1)
      | _ -> loop (i + 1))
  in
  loop (i + 1)

let parse s =
  let transaction_start, _ = object_bounds s "transaction" in
  let customer_start, _ = object_bounds s "customer" in
  let merchant_start, _ = object_bounds s "merchant" in
  let terminal_start, _ = object_bounds s "terminal" in
  let merchant_id = string ~from:merchant_start s "id" in
  let last_transaction =
    let i = value_start s "last_transaction" in
    if String.is_prefix (String.drop_prefix s i) ~prefix:"null"
    then Missing
    else
      Present
        { timestamp = string ~from:i s "timestamp"
        ; km_from_current = number ~from:i s "km_from_current"
        }
  in
  { amount = number ~from:transaction_start s "amount"
  ; installments = int ~from:transaction_start s "installments"
  ; requested_at = string ~from:transaction_start s "requested_at"
  ; customer_avg_amount = number ~from:customer_start s "avg_amount"
  ; tx_count_24h = int ~from:customer_start s "tx_count_24h"
  ; merchant_id
  ; merchant_mcc = string ~from:merchant_start s "mcc"
  ; merchant_avg_amount = number ~from:merchant_start s "avg_amount"
  ; is_online = bool ~from:terminal_start s "is_online"
  ; card_present = bool ~from:terminal_start s "card_present"
  ; km_from_home = number ~from:terminal_start s "km_from_home"
  ; known_merchant = array_contains_string ~from:customer_start s "known_merchants" merchant_id
  ; last_transaction
  }
