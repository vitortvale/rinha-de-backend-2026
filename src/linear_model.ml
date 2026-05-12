type decision = int

let unknown = -1
let unknown_legit_lean = -2
let fraud_decision_score = 405465108
let fraud_logit_threshold = 4074832270
let legit_logit_threshold = -4568685365
let reference_legit_amount_vs_average_max = 10803
let reference_fallback_fraud_hour_max = 12174
let reference_fallback_fraud_merchant_avg_max = 10049

let[@zero_alloc] [@inline always] reference_rule_decide query =
  if query.(2) <= reference_legit_amount_vs_average_max then 2 else unknown
;;

let[@zero_alloc] [@inline always] reference_fallback_rule_decide query =
  if query.(3) <= reference_fallback_fraud_hour_max
     || query.(13) <= reference_fallback_fraud_merchant_avg_max
  then 3
  else unknown
;;

let[@zero_alloc] [@inline always] int_model_score query =
  -21238710739
  + (184670 * query.(0))
  + (315432 * query.(1))
  + (248040 * query.(2))
  - (94460 * query.(3))
  + (17479 * query.(4))
  - (198745 * query.(5))
  + (196833 * query.(6))
  + (278675 * query.(7))
  + (284100 * query.(8))
  + (22108 * query.(9))
  - (7427 * query.(10))
  + (102941 * query.(11))
  + (173763 * query.(12))
  - (16541 * query.(13))
;;

let[@inline always] score query =
  let logit = Float.of_int (int_model_score query) *. 1e-9 in
  1.0 /. (1.0 +. exp (-. logit))
;;

let[@zero_alloc] [@inline always] decide_probability_bucket query =
  if int_model_score query >= fraud_decision_score then 5 else 0
;;

let[@zero_alloc] [@inline always] decide query =
  let reference_decision = reference_rule_decide query in
  if reference_decision >= 0
  then reference_decision
  else (
    let score = int_model_score query in
    if score >= fraud_logit_threshold
    then 5
    else if score < legit_logit_threshold
    then 0
    else (
      let reference_decision = reference_fallback_rule_decide query in
      if reference_decision >= 0
      then reference_decision
      else if score < fraud_decision_score
      then unknown_legit_lean
      else unknown))
;;
