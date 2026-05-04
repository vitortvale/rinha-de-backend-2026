open Core

let dim = 14
let scale = 10000.

let clamp x =
  if Float.(x < 0.) then 0. else if Float.(x > 1.) then 1. else x

let quantize x =
  Int.clamp_exn (Float.iround_nearest_exn ((x +. 1.) *. scale)) ~min:0 ~max:65535

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
  Runtime_json.parse body |> to_float_array config |> Array.map ~f:quantize
