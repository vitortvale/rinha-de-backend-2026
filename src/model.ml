type decision = int

(* Logistic regression trained from the contest reference vectors only.
   The hot path evaluates a fixed-point polynomial over quantized features. *)
let unknown = -1
let p01_logit = -2197224577336
let p03_logit = -847297860387
let p07_logit = 847297860387
let p09_logit = 2197224577336
let fraud_decision_score = 405465108108
let fraud_logit_threshold = 4621615497543
let legit_logit_threshold = -4229991843246
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

let[@zero_alloc] [@inline always] legit_bucket (score : int) =
  if score < p01_logit then 0 else if score < p03_logit then 1 else 2
;;

let[@zero_alloc] [@inline always] fraud_bucket (score : int) =
  if score >= p09_logit then 5 else if score >= p07_logit then 4 else 3
;;

let[@zero_alloc] [@inline always] probability_bucket (score : int) =
  if score < fraud_decision_score then legit_bucket score else fraud_bucket score
;;

let[@zero_alloc] [@inline always] int_model_score query =
  let q0 = query.(0) in
  let q1 = query.(1) in
  let q2 = query.(2) in
  let q3 = query.(3) in
  let q4 = query.(4) in
  let q5 = query.(5) in
  let q6 = query.(6) in
  let q7 = query.(7) in
  let q8 = query.(8) in
  let q9 = query.(9) in
  let q10 = query.(10) in
  let q11 = query.(11) in
  let q12 = query.(12) in
  let q13 = query.(13) in
  -27525116904258
  - (46467126 * q0)
  + (573613731 * q1)
  + (1680269335 * q2)
  - (724743448 * q3)
  + (900762 * q4)
  - (66745261 * q5)
  + (302054795 * q6)
  + (400654504 * q7)
  + (460784120 * q8)
  - (8153414 * q9)
  + (1225737 * q10)
  - (27636191 * q11)
  + (192862429 * q12)
  - (127233793 * q13)
  + (15002 * q0 * q0)
  - (6166 * q1 * q1)
  - (49519 * q2 * q2)
  + (21375 * q3 * q3)
  - (29 * q4 * q4)
  - (26828 * q5 * q5)
  + (3053 * q6 * q6)
  - (57 * q7 * q7)
  - (3140 * q8 * q8)
  + (815 * q9 * q9)
  - (123 * q10 * q10)
  + (2764 * q11 * q11)
  - (2749 * q12 * q12)
  - (761 * q13 * q13)
;;

let score query =
  let logit = Float.of_int (int_model_score query) *. 1e-12 in
  1.0 /. (1.0 +. exp (-. logit))
;;

let[@zero_alloc] [@inline always] decide_probability_bucket query =
  probability_bucket (int_model_score query)
;;

let[@zero_alloc] [@inline always] decide _query =
  unknown
;;
