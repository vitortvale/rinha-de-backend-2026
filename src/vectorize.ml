open Core

let dim = 14
let scale = 10000.

let[@inline always] clamp x =
  if Float.(x < 0.) then 0. else if Float.(x > 1.) then 1. else x

let[@inline always] quantize_clamped x =
  let y = (x +. 1.) *. scale in
  if Float.(y <= 0.)
  then 0
  else if Float.(y >= 65535.)
  then 65535
  else Int.of_float (y +. 0.5)

let[@inline always] int2 s pos =
  ((Char.to_int s.[pos] - Char.to_int '0') * 10)
  + (Char.to_int s.[pos + 1] - Char.to_int '0')

let[@inline always] int4 s pos = (int2 s pos * 100) + int2 s (pos + 2)

let[@inline always] days_from_civil year month day =
  let year = if month <= 2 then year - 1 else year in
  let era = if year >= 0 then year / 400 else (year - 399) / 400 in
  let yoe = year - (era * 400) in
  let month_prime = month + if month > 2 then -3 else 9 in
  let doy = ((153 * month_prime) + 2) / 5 + day - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let[@inline always] timestamp_parts_at s pos =
  let year = int4 s pos in
  let month = int2 s (pos + 5) in
  let day = int2 s (pos + 8) in
  let hour = int2 s (pos + 11) in
  let minute = int2 s (pos + 14) in
  let epoch_days = days_from_civil year month day in
  let day_of_week = Int.( % ) (epoch_days + 3) 7 in
  let epoch_minutes = (((epoch_days * 24) + hour) * 60) + minute in
  hour, day_of_week, epoch_minutes

let[@inline always] is_space = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false

let[@inline always] skip_ws s i =
  let len = String.length s in
  let rec loop i = if i < len && is_space s.[i] then loop (i + 1) else i in
  loop i

let[@inline always] string_equal_at s pos key key_len =
  let rec loop offset =
    offset = key_len || (Char.equal s.[pos + offset] key.[offset] && loop (offset + 1))
  in
  loop 0

let[@inline always] find_key ?(from = 0) s key =
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

let[@inline always] value_start ?(from = 0) s key =
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

let[@inline always] finish_number negative value = if negative then -.value else value

let[@inline always] apply_number_exponent negative value exponent_negative exponent =
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

let[@inline always] number_exponent s len negative j value =
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

let[@inline always] parse_number_value_at s i =
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

let[@inline always] number ?from s key = parse_number_value_at s (value_start ?from s key)

let[@inline always] parse_int_at s i =
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

let[@inline always] int ?from s key = parse_int_at s (value_start ?from s key)

let[@inline always] string_end s i =
  let rec loop j =
    if j >= String.length s then failwith "unterminated string";
    if Char.equal s.[j] '"' && not (Char.equal s.[j - 1] '\\') then j else loop (j + 1)
  in
  loop i

let[@inline always] next_value s i =
  let len = String.length s in
  let rec loop i =
    if i >= len
    then failwith "missing json value"
    else (
      match s.[i] with
      | ':' -> skip_ws s (i + 1)
      | _ -> loop (i + 1))
  in
  loop i

let[@inline always] string_bounds ?from s key =
  let i = value_start ?from s key in
  if not (Char.equal s.[i] '"') then failwith ("expected string: " ^ key);
  let start = i + 1 in
  let stop = string_end s start in
  start, stop - start

let[@inline always] skip_string_value s i =
  if not (Char.equal s.[i] '"') then failwith "expected string";
  string_end s (i + 1) + 1

let[@inline always] string_bounds_at s i =
  if not (Char.equal s.[i] '"') then failwith "expected string";
  let start = i + 1 in
  let stop = string_end s start in
  start, stop - start

let[@inline always] skip_array_value s i =
  if not (Char.equal s.[i] '[') then failwith "expected array";
  let rec loop i =
    if i >= String.length s
    then failwith "unterminated array"
    else (
      match s.[i] with
      | ']' -> i + 1
      | '"' -> loop (string_end s (i + 1) + 1)
      | _ -> loop (i + 1))
  in
  loop (i + 1)

let[@inline always] bool ?from s key =
  let i = value_start ?from s key in
  let prefix = "true" in
  let prefix_len = String.length prefix in
  let rec loop offset =
    offset = prefix_len
    || (Char.equal s.[i + offset] prefix.[offset] && loop (offset + 1))
  in
  i + prefix_len <= String.length s && loop 0

let[@inline always] is_null_at s i =
  i + 4 <= String.length s
  && Char.equal s.[i] 'n'
  && Char.equal s.[i + 1] 'u'
  && Char.equal s.[i + 2] 'l'
  && Char.equal s.[i + 3] 'l'

let[@inline always] object_start ?(from = 0) s key =
  let start = value_start ~from s key in
  if not (Char.equal s.[start] '{') then failwith ("expected object: " ^ key);
  start

let[@inline always] slice_equal_at left_s left right_s right len =
  let rec loop offset =
    offset = len
    || (Char.equal left_s.[left + offset] right_s.[right + offset]
        && loop (offset + 1))
  in
  loop 0

let[@inline always] array_contains_string_slice ?from s key needle_start needle_len =
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

let[@inline always] array_contains_string_slice_at s i needle_start needle_len =
  if not (Char.equal s.[i] '[') then failwith "expected array";
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

let[@inline always] mcc_code_of_slice s start len =
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

let[@inline always] mcc_code ?from s key =
  let start, len = string_bounds ?from s key in
  mcc_code_of_slice s start len

let[@inline always] mcc_code_at s i =
  if not (Char.equal s.[i] '"') then failwith "expected mcc string";
  mcc_code_of_slice s (i + 1) 4

let[@inline always] timestamp_value_parts ?from s key =
  let i = value_start ?from s key in
  if not (Char.equal s.[i] '"') then failwith ("expected string: " ^ key);
  timestamp_parts_at s (i + 1)

let[@inline always] timestamp_parts_value_at s i =
  if not (Char.equal s.[i] '"') then failwith "expected timestamp string";
  timestamp_parts_at s (i + 1)

let[@inline always] bool_at s i = Char.equal s.[i] 't'

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
  let id_start = next_value body 0 in
  let transaction_start = next_value body (skip_string_value body id_start) in
  let amount_start = next_value body transaction_start in
  let installments_start = next_value body amount_start in
  let requested_at_start = next_value body installments_start in
  let amount = parse_number_value_at body amount_start in
  let installments = parse_int_at body installments_start in
  let requested_hour, requested_day_of_week, requested_epoch_minutes =
    timestamp_parts_value_at body requested_at_start
  in
  let customer_start = next_value body (skip_string_value body requested_at_start) in
  let customer_avg_amount_start = next_value body customer_start in
  let tx_count_24h_start = next_value body customer_avg_amount_start in
  let known_merchants_start = next_value body tx_count_24h_start in
  let customer_avg_amount = parse_number_value_at body customer_avg_amount_start in
  let tx_count_24h = parse_int_at body tx_count_24h_start in
  let merchant_start = next_value body (skip_array_value body known_merchants_start) in
  let merchant_id_value_start = next_value body merchant_start in
  let merchant_mcc_start = next_value body (skip_string_value body merchant_id_value_start) in
  let merchant_avg_amount_start = next_value body (skip_string_value body merchant_mcc_start) in
  let merchant_id_start, merchant_id_len = string_bounds_at body merchant_id_value_start in
  let merchant_mcc = mcc_code_at body merchant_mcc_start in
  let merchant_avg_amount = parse_number_value_at body merchant_avg_amount_start in
  let terminal_start = next_value body merchant_avg_amount_start in
  let is_online_start = next_value body terminal_start in
  let card_present_start = next_value body is_online_start in
  let km_from_home_start = next_value body card_present_start in
  let is_online = bool_at body is_online_start in
  let card_present = bool_at body card_present_start in
  let km_from_home = parse_number_value_at body km_from_home_start in
  let known_merchant =
    array_contains_string_slice_at
      body
      known_merchants_start
      merchant_id_start
      merchant_id_len
  in
  let minutes_since_last_transaction, km_from_last_transaction =
    let i = next_value body km_from_home_start in
    if is_null_at body i
    then -1., -1.
    else (
      let timestamp_start = next_value body i in
      let km_from_current_start = next_value body (skip_string_value body timestamp_start) in
      let _, _, last_epoch_minutes = timestamp_parts_value_at body timestamp_start in
      ( clamp
          (Float.of_int (requested_epoch_minutes - last_epoch_minutes)
           /. config.Config.max_minutes)
      , clamp (parse_number_value_at body km_from_current_start /. config.Config.max_km) ))
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

let[@inline always] cget s pos =
  Bigarray.Array1.unsafe_get s.Cstruct.buffer (s.Cstruct.off + pos)
;;

let[@inline always] cint2 s pos =
  ((Char.to_int (cget s pos) - Char.to_int '0') * 10)
  + (Char.to_int (cget s (pos + 1)) - Char.to_int '0')

let[@inline always] cint4 s pos = (cint2 s pos * 100) + cint2 s (pos + 2)

let[@inline always] ctimestamp_parts_at s pos =
  let year = cint4 s pos in
  let month = cint2 s (pos + 5) in
  let day = cint2 s (pos + 8) in
  let hour = cint2 s (pos + 11) in
  let minute = cint2 s (pos + 14) in
  let epoch_days = days_from_civil year month day in
  let day_of_week = Int.( % ) (epoch_days + 3) 7 in
  let epoch_minutes = (((epoch_days * 24) + hour) * 60) + minute in
  hour, day_of_week, epoch_minutes

let[@inline always] cskip_ws s i =
  let len = Cstruct.length s in
  let rec loop i = if i < len && is_space (cget s i) then loop (i + 1) else i in
  loop i

let rec cnumber_exponent_digits s len negative value exponent_negative j exponent =
  if j < len
  then (
    let code = Char.to_int (cget s j) in
    if is_digit_code code
    then (
      cnumber_exponent_digits
        s
        len
        negative
        value
        exponent_negative
        (j + 1)
        ((exponent * 10) + code - Char.to_int '0'))
    else apply_number_exponent negative value exponent_negative exponent)
  else apply_number_exponent negative value exponent_negative exponent

let[@inline always] cnumber_exponent s len negative j value =
  let exp_start = j + 1 in
  if exp_start < len && Char.equal (cget s exp_start) '-'
  then cnumber_exponent_digits s len negative value true (exp_start + 1) 0
  else if exp_start < len && Char.equal (cget s exp_start) '+'
  then cnumber_exponent_digits s len negative value false (exp_start + 1) 0
  else cnumber_exponent_digits s len negative value false exp_start 0

let rec cnumber_fractional_value s len negative j factor acc =
  if j < len
  then (
    let c = cget s j in
    let digit = Char.to_int c - Char.to_int '0' in
    if digit >= 0 && digit <= 9
    then (
      cnumber_fractional_value
        s
        len
        negative
        (j + 1)
        (factor *. 0.1)
        (acc +. (Float.of_int digit *. factor)))
    else if Char.equal c 'e' || Char.equal c 'E'
    then cnumber_exponent s len negative j acc
    else finish_number negative acc)
  else finish_number negative acc

let rec cnumber_integer_value s len negative j acc =
  if j < len
  then (
    let c = cget s j in
    let digit = Char.to_int c - Char.to_int '0' in
    if digit >= 0 && digit <= 9
    then cnumber_integer_value s len negative (j + 1) ((acc *. 10.) +. Float.of_int digit)
    else if Char.equal c '.'
    then cnumber_fractional_value s len negative (j + 1) 0.1 acc
    else if Char.equal c 'e' || Char.equal c 'E'
    then cnumber_exponent s len negative j acc
    else finish_number negative acc)
  else finish_number negative acc

let[@inline always] cparse_number_value_at s i =
  let len = Cstruct.length s in
  if i < len && Char.equal (cget s i) '-'
  then cnumber_integer_value s len true (i + 1) 0.
  else if i < len && Char.equal (cget s i) '+'
  then cnumber_integer_value s len false (i + 1) 0.
  else cnumber_integer_value s len false i 0.

let[@inline always] cparse_int_at s i =
  let len = Cstruct.length s in
  let sign, i =
    if i < len && Char.equal (cget s i) '-'
    then -1, i + 1
    else if i < len && Char.equal (cget s i) '+'
    then 1, i + 1
    else 1, i
  in
  let rec loop j acc =
    if j < len
    then (
      let digit = Char.to_int (cget s j) - Char.to_int '0' in
      if digit >= 0 && digit <= 9 then loop (j + 1) ((acc * 10) + digit) else sign * acc)
    else sign * acc
  in
  loop i 0

let[@inline always] cstring_end s i =
  let len = Cstruct.length s in
  let rec loop j =
    if j >= len then failwith "unterminated string";
    if Char.equal (cget s j) '"' && not (Char.equal (cget s (j - 1)) '\\')
    then j
    else loop (j + 1)
  in
  loop i

let[@inline always] cnext_value s i =
  let len = Cstruct.length s in
  let rec loop i =
    if i >= len
    then failwith "missing json value"
    else (
      match cget s i with
      | ':' -> cskip_ws s (i + 1)
      | _ -> loop (i + 1))
  in
  loop i

let[@inline always] cskip_string_value s i =
  if not (Char.equal (cget s i) '"') then failwith "expected string";
  cstring_end s (i + 1) + 1

let[@inline always] cstring_bounds_at s i =
  if not (Char.equal (cget s i) '"') then failwith "expected string";
  let start = i + 1 in
  let stop = cstring_end s start in
  start, stop - start

let[@inline always] cskip_array_value s i =
  if not (Char.equal (cget s i) '[') then failwith "expected array";
  let len = Cstruct.length s in
  let rec loop i =
    if i >= len
    then failwith "unterminated array"
    else (
      match cget s i with
      | ']' -> i + 1
      | '"' -> loop (cstring_end s (i + 1) + 1)
      | _ -> loop (i + 1))
  in
  loop (i + 1)

let[@inline always] cis_null_at s i =
  i + 4 <= Cstruct.length s
  && Char.equal (cget s i) 'n'
  && Char.equal (cget s (i + 1)) 'u'
  && Char.equal (cget s (i + 2)) 'l'
  && Char.equal (cget s (i + 3)) 'l'

let[@inline always] cslice_equal_at s left right len =
  let rec loop offset =
    offset = len || (Char.equal (cget s (left + offset)) (cget s (right + offset)) && loop (offset + 1))
  in
  loop 0

let[@inline always] carray_contains_string_slice_at s i needle_start needle_len =
  if not (Char.equal (cget s i) '[') then failwith "expected array";
  let len = Cstruct.length s in
  let rec loop i =
    if i >= len
    then false
    else (
      match cget s i with
      | ']' -> false
      | '"' ->
        let start = i + 1 in
        let stop = cstring_end s start in
        if stop - start = needle_len && cslice_equal_at s start needle_start needle_len
        then true
        else loop (stop + 1)
      | _ -> loop (i + 1))
  in
  loop (i + 1)

let[@inline always] cmcc_code_at s i =
  if not (Char.equal (cget s i) '"') then failwith "expected mcc string";
  let start = i + 1 in
  let rec loop offset acc =
    if offset = 4
    then acc
    else (
      let digit = Char.to_int (cget s (start + offset)) - Char.to_int '0' in
      if digit < 0 || digit > 9 then -1 else loop (offset + 1) ((acc * 10) + digit))
  in
  loop 0 0

let[@inline always] ctimestamp_parts_value_at s i =
  if not (Char.equal (cget s i) '"') then failwith "expected timestamp string";
  ctimestamp_parts_at s (i + 1)

let[@inline always] cbool_at s i = Char.equal (cget s i) 't'

let to_quantized_cstruct_into config body query =
  let id_start = cnext_value body 0 in
  let transaction_start = cnext_value body (cskip_string_value body id_start) in
  let amount_start = cnext_value body transaction_start in
  let installments_start = cnext_value body amount_start in
  let requested_at_start = cnext_value body installments_start in
  let amount = cparse_number_value_at body amount_start in
  let installments = cparse_int_at body installments_start in
  let requested_hour, requested_day_of_week, requested_epoch_minutes =
    ctimestamp_parts_value_at body requested_at_start
  in
  let customer_start = cnext_value body (cskip_string_value body requested_at_start) in
  let customer_avg_amount_start = cnext_value body customer_start in
  let tx_count_24h_start = cnext_value body customer_avg_amount_start in
  let known_merchants_start = cnext_value body tx_count_24h_start in
  let customer_avg_amount = cparse_number_value_at body customer_avg_amount_start in
  let tx_count_24h = cparse_int_at body tx_count_24h_start in
  let merchant_start = cnext_value body (cskip_array_value body known_merchants_start) in
  let merchant_id_value_start = cnext_value body merchant_start in
  let merchant_mcc_start = cnext_value body (cskip_string_value body merchant_id_value_start) in
  let merchant_avg_amount_start =
    cnext_value body (cskip_string_value body merchant_mcc_start)
  in
  let merchant_id_start, merchant_id_len = cstring_bounds_at body merchant_id_value_start in
  let merchant_mcc = cmcc_code_at body merchant_mcc_start in
  let merchant_avg_amount = cparse_number_value_at body merchant_avg_amount_start in
  let terminal_start = cnext_value body merchant_avg_amount_start in
  let is_online_start = cnext_value body terminal_start in
  let card_present_start = cnext_value body is_online_start in
  let km_from_home_start = cnext_value body card_present_start in
  let is_online = cbool_at body is_online_start in
  let card_present = cbool_at body card_present_start in
  let km_from_home = cparse_number_value_at body km_from_home_start in
  let known_merchant =
    carray_contains_string_slice_at
      body
      known_merchants_start
      merchant_id_start
      merchant_id_len
  in
  let minutes_since_last_transaction, km_from_last_transaction =
    let i = cnext_value body km_from_home_start in
    if cis_null_at body i
    then -1., -1.
    else (
      let timestamp_start = cnext_value body i in
      let km_from_current_start = cnext_value body (cskip_string_value body timestamp_start) in
      let _, _, last_epoch_minutes = ctimestamp_parts_value_at body timestamp_start in
      ( clamp
          (Float.of_int (requested_epoch_minutes - last_epoch_minutes)
           /. config.Config.max_minutes)
      , clamp (cparse_number_value_at body km_from_current_start /. config.Config.max_km) ))
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

let[@inline always] bget s pos = Bytes.unsafe_get s pos

let[@inline always] bint2 s pos =
  ((Char.to_int (bget s pos) - Char.to_int '0') * 10)
  + (Char.to_int (bget s (pos + 1)) - Char.to_int '0')

let[@inline always] bint4 s pos = (bint2 s pos * 100) + bint2 s (pos + 2)

let[@inline always] btimestamp_parts_at s pos =
  let year = bint4 s pos in
  let month = bint2 s (pos + 5) in
  let day = bint2 s (pos + 8) in
  let hour = bint2 s (pos + 11) in
  let minute = bint2 s (pos + 14) in
  let epoch_days = days_from_civil year month day in
  let day_of_week = Int.( % ) (epoch_days + 3) 7 in
  let epoch_minutes = (((epoch_days * 24) + hour) * 60) + minute in
  hour, day_of_week, epoch_minutes

let[@inline always] bskip_ws s stop i =
  let rec loop i = if i < stop && is_space (bget s i) then loop (i + 1) else i in
  loop i

let rec bnumber_exponent_digits s stop negative value exponent_negative j exponent =
  if j < stop
  then (
    let code = Char.to_int (bget s j) in
    if is_digit_code code
    then (
      bnumber_exponent_digits
        s
        stop
        negative
        value
        exponent_negative
        (j + 1)
        ((exponent * 10) + code - Char.to_int '0'))
    else apply_number_exponent negative value exponent_negative exponent)
  else apply_number_exponent negative value exponent_negative exponent

let[@inline always] bnumber_exponent s stop negative j value =
  let exp_start = j + 1 in
  if exp_start < stop && Char.equal (bget s exp_start) '-'
  then bnumber_exponent_digits s stop negative value true (exp_start + 1) 0
  else if exp_start < stop && Char.equal (bget s exp_start) '+'
  then bnumber_exponent_digits s stop negative value false (exp_start + 1) 0
  else bnumber_exponent_digits s stop negative value false exp_start 0

let rec bnumber_fractional_value s stop negative j factor acc =
  if j < stop
  then (
    let c = bget s j in
    let digit = Char.to_int c - Char.to_int '0' in
    if digit >= 0 && digit <= 9
    then (
      bnumber_fractional_value
        s
        stop
        negative
        (j + 1)
        (factor *. 0.1)
        (acc +. (Float.of_int digit *. factor)))
    else if Char.equal c 'e' || Char.equal c 'E'
    then bnumber_exponent s stop negative j acc
    else finish_number negative acc)
  else finish_number negative acc

let rec bnumber_integer_value s stop negative j acc =
  if j < stop
  then (
    let c = bget s j in
    let digit = Char.to_int c - Char.to_int '0' in
    if digit >= 0 && digit <= 9
    then bnumber_integer_value s stop negative (j + 1) ((acc *. 10.) +. Float.of_int digit)
    else if Char.equal c '.'
    then bnumber_fractional_value s stop negative (j + 1) 0.1 acc
    else if Char.equal c 'e' || Char.equal c 'E'
    then bnumber_exponent s stop negative j acc
    else finish_number negative acc)
  else finish_number negative acc

let[@inline always] bparse_number_value_at s stop i =
  if i < stop && Char.equal (bget s i) '-'
  then bnumber_integer_value s stop true (i + 1) 0.
  else if i < stop && Char.equal (bget s i) '+'
  then bnumber_integer_value s stop false (i + 1) 0.
  else bnumber_integer_value s stop false i 0.

let[@inline always] bparse_int_at s stop i =
  let sign, i =
    if i < stop && Char.equal (bget s i) '-'
    then -1, i + 1
    else if i < stop && Char.equal (bget s i) '+'
    then 1, i + 1
    else 1, i
  in
  let rec loop j acc =
    if j < stop
    then (
      let digit = Char.to_int (bget s j) - Char.to_int '0' in
      if digit >= 0 && digit <= 9 then loop (j + 1) ((acc * 10) + digit) else sign * acc)
    else sign * acc
  in
  loop i 0

let[@inline always] bstring_end s stop i =
  let rec loop j =
    if j >= stop then failwith "unterminated string";
    if Char.equal (bget s j) '"' && not (Char.equal (bget s (j - 1)) '\\')
    then j
    else loop (j + 1)
  in
  loop i

let[@inline always] bnext_value s stop i =
  let rec loop i =
    if i >= stop
    then failwith "missing json value"
    else (
      match bget s i with
      | ':' -> bskip_ws s stop (i + 1)
      | _ -> loop (i + 1))
  in
  loop i

let[@inline always] bskip_string_value s stop i =
  if not (Char.equal (bget s i) '"') then failwith "expected string";
  bstring_end s stop (i + 1) + 1

let[@inline always] bstring_bounds_at s stop i =
  if not (Char.equal (bget s i) '"') then failwith "expected string";
  let start = i + 1 in
  let stop_pos = bstring_end s stop start in
  start, stop_pos - start

let[@zero_alloc] rec bskip_array_loop s stop i =
  if i >= stop
  then failwith "unterminated array"
  else (
    match bget s i with
    | ']' -> i + 1
    | '"' -> bskip_array_loop s stop (bstring_end s stop (i + 1) + 1)
    | _ -> bskip_array_loop s stop (i + 1))

let[@zero_alloc] [@inline always] bskip_array_value s stop i =
  if not (Char.equal (bget s i) '[') then failwith "expected array";
  bskip_array_loop s stop (i + 1)

let[@inline always] bis_null_at s stop i =
  i + 4 <= stop
  && Char.equal (bget s i) 'n'
  && Char.equal (bget s (i + 1)) 'u'
  && Char.equal (bget s (i + 2)) 'l'
  && Char.equal (bget s (i + 3)) 'l'

let[@inline always] bslice_equal_at s left right len =
  let rec loop offset =
    offset = len
    || (Char.equal (bget s (left + offset)) (bget s (right + offset))
        && loop (offset + 1))
  in
  loop 0

let[@zero_alloc] rec barray_contains_string_slice_loop s stop i needle_start needle_len =
  if i >= stop
  then false
  else (
    match bget s i with
    | ']' -> false
    | '"' ->
      let start = i + 1 in
      let stop_pos = bstring_end s stop start in
      if stop_pos - start = needle_len && bslice_equal_at s start needle_start needle_len
      then true
      else barray_contains_string_slice_loop s stop (stop_pos + 1) needle_start needle_len
    | _ -> barray_contains_string_slice_loop s stop (i + 1) needle_start needle_len)

let[@zero_alloc] [@inline always] barray_contains_string_slice_at s stop i needle_start needle_len =
  if not (Char.equal (bget s i) '[') then failwith "expected array";
  barray_contains_string_slice_loop s stop (i + 1) needle_start needle_len

let[@inline always] bmcc_code_at s i =
  if not (Char.equal (bget s i) '"') then failwith "expected mcc string";
  let start = i + 1 in
  let rec loop offset acc =
    if offset = 4
    then acc
    else (
      let digit = Char.to_int (bget s (start + offset)) - Char.to_int '0' in
      if digit < 0 || digit > 9 then -1 else loop (offset + 1) ((acc * 10) + digit))
  in
  loop 0 0

let[@inline always] btimestamp_parts_value_at s i =
  if not (Char.equal (bget s i) '"') then failwith "expected timestamp string";
  btimestamp_parts_at s (i + 1)

let[@inline always] bbool_at s i = Char.equal (bget s i) 't'

let to_quantized_bytes_into config body body_start body_len query =
  let body_stop = body_start + body_len in
  let id_start = bnext_value body body_stop body_start in
  let transaction_start = bnext_value body body_stop (bskip_string_value body body_stop id_start) in
  let amount_start = bnext_value body body_stop transaction_start in
  let installments_start = bnext_value body body_stop amount_start in
  let requested_at_start = bnext_value body body_stop installments_start in
  let amount = bparse_number_value_at body body_stop amount_start in
  let installments = bparse_int_at body body_stop installments_start in
  let requested_hour, requested_day_of_week, requested_epoch_minutes =
    btimestamp_parts_value_at body requested_at_start
  in
  let customer_start =
    bnext_value body body_stop (bskip_string_value body body_stop requested_at_start)
  in
  let customer_avg_amount_start = bnext_value body body_stop customer_start in
  let tx_count_24h_start = bnext_value body body_stop customer_avg_amount_start in
  let known_merchants_start = bnext_value body body_stop tx_count_24h_start in
  let customer_avg_amount = bparse_number_value_at body body_stop customer_avg_amount_start in
  let tx_count_24h = bparse_int_at body body_stop tx_count_24h_start in
  let merchant_start =
    bnext_value body body_stop (bskip_array_value body body_stop known_merchants_start)
  in
  let merchant_id_value_start = bnext_value body body_stop merchant_start in
  let merchant_mcc_start =
    bnext_value body body_stop (bskip_string_value body body_stop merchant_id_value_start)
  in
  let merchant_avg_amount_start =
    bnext_value body body_stop (bskip_string_value body body_stop merchant_mcc_start)
  in
  let merchant_id_start, merchant_id_len =
    bstring_bounds_at body body_stop merchant_id_value_start
  in
  let merchant_mcc = bmcc_code_at body merchant_mcc_start in
  let merchant_avg_amount = bparse_number_value_at body body_stop merchant_avg_amount_start in
  let terminal_start = bnext_value body body_stop merchant_avg_amount_start in
  let is_online_start = bnext_value body body_stop terminal_start in
  let card_present_start = bnext_value body body_stop is_online_start in
  let km_from_home_start = bnext_value body body_stop card_present_start in
  let is_online = bbool_at body is_online_start in
  let card_present = bbool_at body card_present_start in
  let km_from_home = bparse_number_value_at body body_stop km_from_home_start in
  let known_merchant =
    barray_contains_string_slice_at
      body
      body_stop
      known_merchants_start
      merchant_id_start
      merchant_id_len
  in
  let minutes_since_last_transaction, km_from_last_transaction =
    let i = bnext_value body body_stop km_from_home_start in
    if bis_null_at body body_stop i
    then -1., -1.
    else (
      let timestamp_start = bnext_value body body_stop i in
      let km_from_current_start =
        bnext_value body body_stop (bskip_string_value body body_stop timestamp_start)
      in
      let _, _, last_epoch_minutes = btimestamp_parts_value_at body timestamp_start in
      ( clamp
          (Float.of_int (requested_epoch_minutes - last_epoch_minutes)
           /. config.Config.max_minutes)
      , clamp (bparse_number_value_at body body_stop km_from_current_start /. config.Config.max_km) ))
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
