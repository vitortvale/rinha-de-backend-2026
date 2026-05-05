type t

val k : int
val load : string -> t
val score_frauds : t -> int array -> int
val score : t -> int array -> float
