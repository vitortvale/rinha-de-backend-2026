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

let string_equal_at s pos key key_len =
  let rec loop offset =
    offset = key_len || (Char.equal s.[pos + offset] key.[offset] && loop (offset + 1))
  in
  loop 0

let find_key ?(from = 0) s key =
  let len = String.length s in
  let key_len = String.length key in
  let stop = len - key_len - 2 in
  let rec loop i =
    if i > stop
    then failwith ("missing json key: " ^ key)
    else if Char.equal s.[i] '"'
            && Char.equal s.[i + key_len + 1] '"'
            && string_equal_at s (i + 1) key key_len
    then i + key_len + 2
    else loop (i + 1)
  in
  loop from

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

let is_digit_code code = code >= Char.to_int '0' && code <= Char.to_int '9'

let finish_number negative value = if negative then -.value else value

let apply_number_exponent negative value exponent_negative exponent =
  let exponent = if exponent_negative then -exponent else exponent in
  finish_number negative (value *. (10. ** Float.of_int exponent))

let rec number_exponent_digits s len negative value exponent_negative j exponent =
  if j < len
  then (
    let code = Char.to_int s.[j] in
    if is_digit_code code
    then (
      number_exponent_digits
        s
        len
        negative
        value
        exponent_negative
        (j + 1)
        ((exponent * 10) + code - Char.to_int '0'))
    else apply_number_exponent negative value exponent_negative exponent)
  else apply_number_exponent negative value exponent_negative exponent

let number_exponent s len negative j value =
  let exp_start = j + 1 in
  if exp_start < len && Char.equal s.[exp_start] '-'
  then number_exponent_digits s len negative value true (exp_start + 1) 0
  else if exp_start < len && Char.equal s.[exp_start] '+'
  then number_exponent_digits s len negative value false (exp_start + 1) 0
  else number_exponent_digits s len negative value false exp_start 0

let rec number_fractional_value s len negative j factor acc =
  if j < len
  then (
    let c = s.[j] in
    let digit = Char.to_int c - Char.to_int '0' in
    if digit >= 0 && digit <= 9
    then (
      number_fractional_value
        s
        len
        negative
        (j + 1)
        (factor *. 0.1)
        (acc +. (Float.of_int digit *. factor)))
    else if Char.equal c 'e' || Char.equal c 'E'
    then number_exponent s len negative j acc
    else finish_number negative acc)
  else finish_number negative acc

let rec number_integer_value s len negative j acc =
  if j < len
  then (
    let c = s.[j] in
    let digit = Char.to_int c - Char.to_int '0' in
    if digit >= 0 && digit <= 9
    then number_integer_value s len negative (j + 1) ((acc *. 10.) +. Float.of_int digit)
    else if Char.equal c '.'
    then number_fractional_value s len negative (j + 1) 0.1 acc
    else if Char.equal c 'e' || Char.equal c 'E'
    then number_exponent s len negative j acc
    else finish_number negative acc)
  else finish_number negative acc

let parse_number_value_at s i =
  let len = String.length s in
  if i < len && Char.equal s.[i] '-'
  then number_integer_value s len true (i + 1) 0.
  else if i < len && Char.equal s.[i] '+'
  then number_integer_value s len false (i + 1) 0.
  else number_integer_value s len false i 0.

let rec number_integer_part s len j acc =
  if j < len
  then (
    let digit = Char.to_int s.[j] - Char.to_int '0' in
    if digit >= 0 && digit <= 9
    then number_integer_part s len (j + 1) ((acc *. 10.) +. Float.of_int digit)
    else j, acc)
  else j, acc

let rec number_fractional_part s len j factor acc =
  if j < len
  then (
    let digit = Char.to_int s.[j] - Char.to_int '0' in
    if digit >= 0 && digit <= 9
    then (
      number_fractional_part
        s
        len
        (j + 1)
        (factor *. 0.1)
        (acc +. (Float.of_int digit *. factor)))
    else j, acc)
  else j, acc

let rec number_exponent_part s len j acc =
  if j < len
  then (
    let code = Char.to_int s.[j] in
    if is_digit_code code
    then number_exponent_part s len (j + 1) ((acc * 10) + code - Char.to_int '0')
    else j, acc)
  else j, acc

let parse_number_at s i =
  let len = String.length s in
  let negative, i =
    if i < len && Char.equal s.[i] '-'
    then true, i + 1
    else if i < len && Char.equal s.[i] '+'
    then false, i + 1
    else false, i
  in
  let j, value = number_integer_part s len i 0. in
  let j, value =
    if j < len && Char.equal s.[j] '.'
    then number_fractional_part s len (j + 1) 0.1 value
    else j, value
  in
  let j, value =
    if j < len && (Char.equal s.[j] 'e' || Char.equal s.[j] 'E')
    then (
      let exp_start = j + 1 in
      let exp_negative, exp_start =
        if exp_start < len && Char.equal s.[exp_start] '-'
        then true, exp_start + 1
        else if exp_start < len && Char.equal s.[exp_start] '+'
        then false, exp_start + 1
        else false, exp_start
      in
      let j, exponent = number_exponent_part s len exp_start 0 in
      let exponent = if exp_negative then -exponent else exponent in
      j, value *. (10. ** Float.of_int exponent))
    else j, value
  in
  (if negative then -.value else value), j

let number ?from s key = parse_number_value_at s (value_start ?from s key)

let parse_int_at s i =
  let len = String.length s in
  let sign, i =
    if i < len && Char.equal s.[i] '-'
    then -1, i + 1
    else if i < len && Char.equal s.[i] '+'
    then 1, i + 1
    else 1, i
  in
  let rec loop j acc =
    if j < len
    then (
      let digit = Char.to_int s.[j] - Char.to_int '0' in
      if digit >= 0 && digit <= 9 then loop (j + 1) ((acc * 10) + digit) else sign * acc)
    else sign * acc
  in
  loop i 0

let int ?from s key = parse_int_at s (value_start ?from s key)

let string_end s i =
  let rec loop j =
    if j >= String.length s then failwith "unterminated string";
    if Char.equal s.[j] '"' && not (Char.equal s.[j - 1] '\\') then j else loop (j + 1)
  in
  loop i

let string_bounds ?from s key =
  let i = value_start ?from s key in
  if not (Char.equal s.[i] '"') then failwith ("expected string: " ^ key);
  let start = i + 1 in
  let stop = string_end s start in
  start, stop - start

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

let object_start ?(from = 0) s key =
  let start = value_start ~from s key in
  if not (Char.equal s.[start] '{') then failwith ("expected object: " ^ key);
  start

let slice_equal_at left_s left right_s right len =
  let rec loop offset =
    offset = len
    || (Char.equal left_s.[left + offset] right_s.[right + offset]
        && loop (offset + 1))
  in
  loop 0

let array_contains_string_slice ?from s key needle_start needle_len =
  let i = value_start ?from s key in
  if not (Char.equal s.[i] '[') then failwith ("expected array: " ^ key);
  let rec loop i =
    if i >= String.length s
    then false
    else (
      match s.[i] with
      | ']' -> false
      | '"' ->
        let start = i + 1 in
        let stop = string_end s start in
        if stop - start = needle_len && slice_equal_at s start s needle_start needle_len
        then true
        else loop (stop + 1)
      | _ -> loop (i + 1))
  in
  loop (i + 1)

let mcc_code_of_slice s start len =
  if len <> 4
  then -1
  else (
    let rec loop i acc =
      if i = len
      then acc
      else (
        let digit = Char.to_int s.[start + i] - Char.to_int '0' in
        if digit < 0 || digit > 9 then -1 else loop (i + 1) ((acc * 10) + digit))
    in
    loop 0 0)

let mcc_code ?from s key =
  let start, len = string_bounds ?from s key in
  mcc_code_of_slice s start len

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

let to_quantized_into config body query =
  let transaction_start = object_start body "transaction" in
  let customer_start = object_start body "customer" in
  let merchant_start = object_start body "merchant" in
  let terminal_start = object_start body "terminal" in
  let amount = number ~from:transaction_start body "amount" in
  let installments = int ~from:transaction_start body "installments" in
  let requested_hour, requested_day_of_week, requested_epoch_minutes =
    timestamp_value_parts ~from:transaction_start body "requested_at"
  in
  let customer_avg_amount = number ~from:customer_start body "avg_amount" in
  let tx_count_24h = int ~from:customer_start body "tx_count_24h" in
  let merchant_id_start, merchant_id_len = string_bounds ~from:merchant_start body "id" in
  let merchant_mcc = mcc_code ~from:merchant_start body "mcc" in
  let merchant_avg_amount = number ~from:merchant_start body "avg_amount" in
  let is_online = bool ~from:terminal_start body "is_online" in
  let card_present = bool ~from:terminal_start body "card_present" in
  let km_from_home = number ~from:terminal_start body "km_from_home" in
  let known_merchant =
    array_contains_string_slice
      ~from:customer_start
      body
      "known_merchants"
      merchant_id_start
      merchant_id_len
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
  query.(0) <- quantize_clamped (clamp (amount /. config.Config.max_amount));
  query.(1)
  <- quantize_clamped
       (clamp (Float.of_int installments /. config.Config.max_installments));
  query.(2)
  <- quantize_clamped
       (clamp
          ((amount /. Float.max customer_avg_amount 0.000001)
           /. config.Config.amount_vs_avg_ratio));
  query.(3) <- quantize_clamped (Float.of_int requested_hour /. 23.);
  query.(4) <- quantize_clamped (Float.of_int requested_day_of_week /. 6.);
  query.(5) <- quantize_clamped minutes_since_last_transaction;
  query.(6) <- quantize_clamped km_from_last_transaction;
  query.(7) <- quantize_clamped (clamp (km_from_home /. config.Config.max_km));
  query.(8)
  <- quantize_clamped
       (clamp (Float.of_int tx_count_24h /. config.Config.max_tx_count_24h));
  query.(9) <- quantize_clamped (if is_online then 1. else 0.);
  query.(10) <- quantize_clamped (if card_present then 1. else 0.);
  query.(11) <- quantize_clamped (if known_merchant then 0. else 1.);
  query.(12) <- quantize_clamped (Config.mcc_risk_code config merchant_mcc);
  query.(13)
  <- quantize_clamped
       (clamp (merchant_avg_amount /. config.Config.max_merchant_avg_amount))

let to_quantized config body =
  let query = Array.create ~len:dim 0 in
  to_quantized_into config body query;
  query
