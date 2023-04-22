type t

val make : int -> t
val search : int -> t -> int
val insert : int -> t -> t
val delete : int -> t -> t
val clear : t -> t
val set_to_list : t -> int list
