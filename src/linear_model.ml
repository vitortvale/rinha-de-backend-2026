type decision =
  | Fraud
  | Legit
  | Unknown

(* logit thresholds — sigmoid-free hot path *)
let fraud_logit_threshold = 4.0748352316358663
let legit_logit_threshold = -4.568684979847542
let reference_legit_amount_vs_average_max = 10803
let reference_fallback_fraud_hour_max = 12174
let reference_fallback_fraud_merchant_avg_max = 10049

let[@inline always] feature query i =
  (Float.of_int query.(i) *. 0.0001) -. 1.0
;;

let[@zero_alloc] [@inline always] reference_rule_decide query =
  if query.(2) <= reference_legit_amount_vs_average_max then Legit else Unknown
;;

let[@zero_alloc] [@inline always] reference_fallback_rule_decide query =
  if query.(3) <= reference_fallback_fraud_hour_max
     || query.(13) <= reference_fallback_fraud_merchant_avg_max
  then Fraud
  else Unknown
;;

let[@inline always] model_logit x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 =
  (
    -6.1700261685979356
    +. (1.8467045898303502 *. x0)
    +. (3.1543242518123691 *. x1)
    +. (2.4804005543069567 *. x2)
    -. (0.94460100706859418 *. x3)
    +. (0.17479143000689881 *. x4)
    -. (1.9874519722686619 *. x5)
    +. (1.9683265027752996 *. x6)
    +. (2.7867510830357438 *. x7)
    +. (2.8410026260722758 *. x8)
    +. (0.22108133704432259 *. x9)
    -. (0.07427495701295303 *. x10)
    +. (1.0294108513279079 *. x11)
    +. (1.7376264136204973 *. x12)
    -. (0.16540713299329268 *. x13)
  )
;;

let score query =
  let x0 = feature query 0 in
  let x1 = feature query 1 in
  let x2 = feature query 2 in
  let x3 = feature query 3 in
  let x4 = feature query 4 in
  let x5 = feature query 5 in
  let x6 = feature query 6 in
  let x7 = feature query 7 in
  let x8 = feature query 8 in
  let x9 = feature query 9 in
  let x10 = feature query 10 in
  let x11 = feature query 11 in
  let x12 = feature query 12 in
  let x13 = feature query 13 in
  let logit = model_logit x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 in
  1.0 /. (1.0 +. exp (-. logit))
;;

let[@inline always] decide query =
  match reference_rule_decide query with
  | Legit -> Legit
  | Fraud -> Fraud
  | Unknown ->
    let x0 = feature query 0 in
    let x1 = feature query 1 in
    let x2 = feature query 2 in
    let x3 = feature query 3 in
    let x4 = feature query 4 in
    let x5 = feature query 5 in
    let x6 = feature query 6 in
    let x7 = feature query 7 in
    let x8 = feature query 8 in
    let x9 = feature query 9 in
    let x10 = feature query 10 in
    let x11 = feature query 11 in
    let x12 = feature query 12 in
    let x13 = feature query 13 in
    let logit = model_logit x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 in
    if logit > fraud_logit_threshold then Fraud
    else if logit < legit_logit_threshold then Legit
    else reference_fallback_rule_decide query
;;
