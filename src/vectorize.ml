open Core

let dim = 14
let scale = 10000.

let clamp x =
  if Float.(x < 0.) then 0. else if Float.(x > 1.) then 1. else x

let quantize x =
  Int.clamp_exn (Float.iround_nearest_exn ((x +. 1.) *. scale)) ~min:0 ~max:65535

let to_float_array config tx =
  let vector = Array.create ~len:dim 0. in
  vector.(0) <- clamp (tx.Runtime_json.amount /. config.Config.max_amount);
  vector.(1) <- clamp (Float.of_int tx.installments /. config.max_installments);
  vector.(2)
  <- clamp
       ((tx.amount /. Float.max tx.customer_avg_amount 0.000001) /. config.amount_vs_avg_ratio);
  vector.(3) <- Float.of_int (Time_util.hour tx.requested_at) /. 23.;
  vector.(4) <- Float.of_int (Time_util.day_of_week tx.requested_at) /. 6.;
  (match tx.last_transaction with
   | Runtime_json.Missing ->
     vector.(5) <- -1.;
     vector.(6) <- -1.
   | Runtime_json.Present last ->
     let minutes =
       Time_util.epoch_minutes tx.requested_at - Time_util.epoch_minutes last.timestamp
     in
     vector.(5) <- clamp (Float.of_int minutes /. config.max_minutes);
     vector.(6) <- clamp (last.km_from_current /. config.max_km));
  vector.(7) <- clamp (tx.km_from_home /. config.max_km);
  vector.(8) <- clamp (Float.of_int tx.tx_count_24h /. config.max_tx_count_24h);
  vector.(9) <- (if tx.is_online then 1. else 0.);
  vector.(10) <- (if tx.card_present then 1. else 0.);
  vector.(11) <- (if tx.known_merchant then 0. else 1.);
  vector.(12) <- Config.mcc_risk config tx.merchant_mcc;
  vector.(13) <- clamp (tx.merchant_avg_amount /. config.max_merchant_avg_amount);
  vector

let to_quantized config body =
  Runtime_json.parse body |> to_float_array config |> Array.map ~f:quantize
