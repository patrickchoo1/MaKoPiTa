type t
(** The type of an sparse_arraylist *)

val make : int -> t
(** [make n] is an arraylist with initial capacity of n. *)

val search : int -> t -> int
(** [search x arr] searches for [x] in the set in [arr]. *)

val insert : int -> t -> t
(** [insert x arr] inserts [x] into the set in [arr]. *)

val delete : int -> t -> t
(** [insert x arr] removes [x] from the set in [arr]. *)

val clear : t -> t
(** [clear arr] clears all elements form the set in [arr]. *)

val set_to_list : t -> int list
(** [set_to_list arr] converts the set in [arr] to an int list. *)
