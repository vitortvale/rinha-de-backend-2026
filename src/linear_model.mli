type decision =
  | Fraud
  | Legit
  | Unknown

val score : int array -> float
val decide : int array -> decision
