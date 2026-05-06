open Core

type t =
  { max_amount : float
  ; max_installments : float
  ; amount_vs_avg_ratio : float
  ; max_minutes : float
  ; max_km : float
  ; max_tx_count_24h : float
  ; max_merchant_avg_amount : float
  ; mcc_risk : float array
  }

val load : string -> t
val mcc_risk : t -> string -> float
val mcc_risk_code : t -> int -> float
