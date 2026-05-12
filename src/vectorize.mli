val dim : int
val to_float_array : Config.t -> Runtime_json.transaction -> float array
val to_quantized : Config.t -> string -> int array
val to_quantized_into : Config.t -> string -> int array -> unit
val to_quantized_cstruct_into : Config.t -> Cstruct.t -> int array -> unit
val to_quantized_bytes_into : Config.t -> bytes -> int -> int -> int array -> unit
