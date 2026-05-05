type t

val k : int
val load : string -> t
val prewarm : t -> unit
val score_frauds : t -> int array -> int
val score : t -> int array -> float
