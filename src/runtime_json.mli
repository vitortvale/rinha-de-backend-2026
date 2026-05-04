type last_transaction =
  | Missing
  | Present of
      { timestamp : string
      ; km_from_current : float
      }

type transaction =
  { amount : float
  ; installments : int
  ; requested_at : string
  ; customer_avg_amount : float
  ; tx_count_24h : int
  ; merchant_id : string
  ; merchant_mcc : string
  ; merchant_avg_amount : float
  ; is_online : bool
  ; card_present : bool
  ; km_from_home : float
  ; known_merchant : bool
  ; last_transaction : last_transaction
  }

val parse : string -> transaction
