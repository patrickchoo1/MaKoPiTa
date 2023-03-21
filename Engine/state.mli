type t

val init_state : Yojson.Basic.t -> t
val update : t -> int * int -> bool -> t
val render : t -> unit
val rand_state : unit -> t
