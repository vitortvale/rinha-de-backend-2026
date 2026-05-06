open Core

let dim = 14
let scale = 10000.

let clamp x =
  if Float.(x < 0.) then 0. else if Float.(x > 1.) then 1. else x

let quantize_clamped x =
  let y = (x +. 1.) *. scale in
  if Float.(y <= 0.)
  then 0
  else if Float.(y >= 65535.)
  then 65535
  else Int.of_float (y +. 0.5)

let int2 s pos =
  ((Char.to_int s.[pos] - Char.to_int '0') * 10)
  + (Char.to_int s.[pos + 1] - Char.to_int '0')

let int4 s pos = (int2 s pos * 100) + int2 s (pos + 2)

let days_from_civil year month day =
  let year = if month <= 2 then year - 1 else year in
  let era = if year >= 0 then year / 400 else (year - 399) / 400 in
  let yoe = year - (era * 400) in
  let month_prime = month + if month > 2 then -3 else 9 in
  let doy = ((153 * month_prime) + 2) / 5 + day - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let timestamp_parts_at s pos =
  let year = int4 s pos in
  let month = int2 s (pos + 5) in
  let day = int2 s (pos + 8) in
  let hour = int2 s (pos + 11) in
  let minute = int2 s (pos + 14) in
  let epoch_days = days_from_civil year month day in
  let day_of_week = Int.( % ) (epoch_days + 3) 7 in
  let epoch_minutes = (((epoch_days * 24) + hour) * 60) + minute in
  hour, day_of_week, epoch_minutes

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
  let rec colon i =
    if i >= len
    then failwith "missing colon"
    else if Char.equal s.[i] ':'
    then i + 1
    else colon (i + 1)
  in
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
  let prefix = "true" in
  let prefix_len = String.length prefix in
  let rec loop offset =
    offset = prefix_len
    || (Char.equal s.[i + offset] prefix.[offset] && loop (offset + 1))
  in
  i + prefix_len <= String.length s && loop 0

let is_null_at s i =
  i + 4 <= String.length s
  && Char.equal s.[i] 'n'
  && Char.equal s.[i + 1] 'u'
  && Char.equal s.[i + 2] 'l'
  && Char.equal s.[i + 3] 'l'

let object_bounds ?(from = 0) s key =
  let start = value_start ~from s key in
  if not (Char.equal s.[start] '{') then failwith ("expected object: " ^ key);
  let rec loop i depth in_string escaped =
    if i >= String.length s
    then failwith "unterminated object"
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
          if depth = 0 then start, i else loop (i + 1) depth false false
        | _ -> loop (i + 1) depth false false))
  in
  loop start 0 false false

let array_contains_string ?from s key needle =
  let i = value_start ?from s key in
  if not (Char.equal s.[i] '[') then failwith ("expected array: " ^ key);
  let rec loop i =
    if i >= String.length s
    then false
    else (
      match s.[i] with
      | ']' -> false
      | '"' ->
        let j = Option.value_exn (String.index_from s (i + 1) '"') in
        if String.equal needle (String.sub s ~pos:(i + 1) ~len:(j - i - 1))
        then true
        else loop (j + 1)
      | _ -> loop (i + 1))
  in
  loop (i + 1)

let timestamp_value_parts ?from s key =
  let i = value_start ?from s key in
  if not (Char.equal s.[i] '"') then failwith ("expected string: " ^ key);
  timestamp_parts_at s (i + 1)

let amount config tx = clamp (tx.Runtime_json.amount /. config.Config.max_amount)

let installments config tx =
  clamp (Float.of_int tx.Runtime_json.installments /. config.Config.max_installments)

let amount_vs_average config tx =
  clamp
    ((tx.Runtime_json.amount /. Float.max tx.customer_avg_amount 0.000001)
     /. config.Config.amount_vs_avg_ratio)

let requested_hour _config tx =
  Float.of_int (Time_util.hour tx.Runtime_json.requested_at) /. 23.

let requested_day_of_week _config tx =
  Float.of_int (Time_util.day_of_week tx.Runtime_json.requested_at) /. 6.

let minutes_since_last_transaction config tx =
  match tx.Runtime_json.last_transaction with
  | Missing -> -1.
  | Present last ->
    let minutes =
      Time_util.epoch_minutes tx.requested_at - Time_util.epoch_minutes last.timestamp
    in
    clamp (Float.of_int minutes /. config.Config.max_minutes)

let km_from_last_transaction config tx =
  match tx.Runtime_json.last_transaction with
  | Missing -> -1.
  | Present last -> clamp (last.km_from_current /. config.Config.max_km)

let km_from_home config tx =
  clamp (tx.Runtime_json.km_from_home /. config.Config.max_km)

let tx_count_24h config tx =
  clamp
    (Float.of_int tx.Runtime_json.tx_count_24h /. config.Config.max_tx_count_24h)

let is_online _config tx = if tx.Runtime_json.is_online then 1. else 0.
let card_present _config tx = if tx.Runtime_json.card_present then 1. else 0.
let unknown_merchant _config tx = if tx.Runtime_json.known_merchant then 0. else 1.

let mcc_risk config tx = Config.mcc_risk config tx.Runtime_json.merchant_mcc

let merchant_average_amount config tx =
  clamp (tx.Runtime_json.merchant_avg_amount /. config.Config.max_merchant_avg_amount)

let to_float_array config tx =
  [| amount config tx
   ; installments config tx
   ; amount_vs_average config tx
   ; requested_hour config tx
   ; requested_day_of_week config tx
   ; minutes_since_last_transaction config tx
   ; km_from_last_transaction config tx
   ; km_from_home config tx
   ; tx_count_24h config tx
   ; is_online config tx
   ; card_present config tx
   ; unknown_merchant config tx
   ; mcc_risk config tx
   ; merchant_average_amount config tx
  |]

let to_quantized config body =
  let transaction_start, _ = object_bounds body "transaction" in
  let customer_start, _ = object_bounds body "customer" in
  let merchant_start, _ = object_bounds body "merchant" in
  let terminal_start, _ = object_bounds body "terminal" in
  let amount = number ~from:transaction_start body "amount" in
  let installments = int ~from:transaction_start body "installments" in
  let requested_hour, requested_day_of_week, requested_epoch_minutes =
    timestamp_value_parts ~from:transaction_start body "requested_at"
  in
  let customer_avg_amount = number ~from:customer_start body "avg_amount" in
  let tx_count_24h = int ~from:customer_start body "tx_count_24h" in
  let merchant_id = string ~from:merchant_start body "id" in
  let merchant_mcc = string ~from:merchant_start body "mcc" in
  let merchant_avg_amount = number ~from:merchant_start body "avg_amount" in
  let is_online = bool ~from:terminal_start body "is_online" in
  let card_present = bool ~from:terminal_start body "card_present" in
  let km_from_home = number ~from:terminal_start body "km_from_home" in
  let known_merchant =
    array_contains_string ~from:customer_start body "known_merchants" merchant_id
  in
  let minutes_since_last_transaction, km_from_last_transaction =
    let i = value_start body "last_transaction" in
    if is_null_at body i
    then -1., -1.
    else (
      let _, _, last_epoch_minutes = timestamp_value_parts ~from:i body "timestamp" in
      ( clamp
          (Float.of_int (requested_epoch_minutes - last_epoch_minutes)
           /. config.Config.max_minutes)
      , clamp (number ~from:i body "km_from_current" /. config.Config.max_km) ))
  in
  [| quantize_clamped (clamp (amount /. config.Config.max_amount))
   ; quantize_clamped
       (clamp (Float.of_int installments /. config.Config.max_installments))
   ; quantize_clamped
       (clamp
          ((amount /. Float.max customer_avg_amount 0.000001)
           /. config.Config.amount_vs_avg_ratio))
   ; quantize_clamped (Float.of_int requested_hour /. 23.)
   ; quantize_clamped (Float.of_int requested_day_of_week /. 6.)
   ; quantize_clamped minutes_since_last_transaction
   ; quantize_clamped km_from_last_transaction
   ; quantize_clamped (clamp (km_from_home /. config.Config.max_km))
   ; quantize_clamped
       (clamp (Float.of_int tx_count_24h /. config.Config.max_tx_count_24h))
   ; quantize_clamped (if is_online then 1. else 0.)
   ; quantize_clamped (if card_present then 1. else 0.)
   ; quantize_clamped (if known_merchant then 0. else 1.)
   ; quantize_clamped (Config.mcc_risk config merchant_mcc)
   ; quantize_clamped
       (clamp (merchant_avg_amount /. config.Config.max_merchant_avg_amount))
  |]
