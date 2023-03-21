type t

val init_target : Yojson.Basic.t -> t
val get_id : t -> int
val is_hit : t -> int * int -> bool
val render : t -> unit
val rand_target : unit -> t
