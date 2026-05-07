type decision =
  | Fraud
  | Legit
  | Unknown

(* Replace these variables with values printed by model/train_linear_model.py. *)
let weights =
  [| 4.701838295964801
   ; 4.692122461633441
   ; 0.9073321589615767
   ; -0.21792110737699524
   ; -0.00420773294131206
   ; -6.6310693893144705
   ; 6.359460615838385
   ; 4.651360908094851
   ; 4.358723440058607
   ; 0.15925248268953568
   ; -0.029668357818041773
   ; 0.5805390405589522
   ; 1.1366650666228457
   ; -17.44972962841129
  |]
;;

let bias = -6.355306601117298
let threshold_low = -4.1863395770931655
let threshold_high = 5.661241530793651

let[@inline always] feature query i =
  (Float.of_int query.(i) *. 0.0001) -. 1.0
;;

let score query =
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

let decide query =
  let z = score query in
  if z > threshold_high then Fraud else if z < threshold_low then Legit else Unknown
;;
