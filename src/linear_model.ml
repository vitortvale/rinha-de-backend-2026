type decision =
  | Fraud
  | Legit
  | Unknown

(* Replace these variables with values printed by model/train_linear_model.py. *)
let fraud_weights =
  [| 6.072556904670579
   ; 5.040057074448095
   ; 0.10149256795957731
   ; -3.9987867303520903
   ; -0.019075271019194116
   ; -4.797903071146745
   ; 4.105074240537634
   ; 5.470676895443625
   ; 4.723722920325428
   ; 0.3301675190010143
   ; -0.04641870306558599
   ; 0.7052324400258174
   ; 1.4263918244173244
   ; -0.6365925224790286
  |]
;;

let legit_weights =
  [| 3.048414194816796
   ; 10.365683620408127
   ; 4.592121896996708
   ; 0.41461482325247906
   ; 0.15699065118262895
   ; -8.598636696603853
   ; 8.669197675871514
   ; 10.205408968869982
   ; 9.306892131368606
   ; 0.5755969229442458
   ; 0.2388424784731271
   ; 1.1923435858666185
   ; 1.865860773199138
   ; -1.0013798015443998
  |]
;;

let fraud_bias = -11.430811342678382
let legit_bias = -7.690979710604093
let fraud_threshold_high = -0.3204550453657539
let legit_threshold_low = -1.4418117649706923

let[@inline always] feature query i =
  (Float.of_int query.(i) *. 0.0001) -. 1.0
;;

let score_with weights bias query =
  bias
  +. (weights.(0) *. feature query 0)
  +. (weights.(1) *. feature query 1)
  +. (weights.(2) *. feature query 2)
  +. (weights.(3) *. feature query 3)
  +. (weights.(4) *. feature query 4)
  +. (weights.(5) *. feature query 5)
  +. (weights.(6) *. feature query 6)
  +. (weights.(7) *. feature query 7)
  +. (weights.(8) *. feature query 8)
  +. (weights.(9) *. feature query 9)
  +. (weights.(10) *. feature query 10)
  +. (weights.(11) *. feature query 11)
  +. (weights.(12) *. feature query 12)
  +. (weights.(13) *. feature query 13)
;;

let score query = score_with fraud_weights fraud_bias query

let decide query =
  if score_with fraud_weights fraud_bias query > fraud_threshold_high
  then Fraud
  else if score_with legit_weights legit_bias query < legit_threshold_low
  then Legit
  else Unknown
;;
