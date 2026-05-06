type t

val k : int
val load : string -> t
val load_for_bench : string -> t
val prewarm : t -> unit
val score_frauds_vp : t -> int array -> int
val score_frauds_centroid_ivf : t -> int array -> int
type scorer
val create_scorer : t -> scorer
val score_frauds_with_scorer : t -> scorer -> int array -> int
val score_frauds : t -> int array -> int
val score : t -> int array -> float
