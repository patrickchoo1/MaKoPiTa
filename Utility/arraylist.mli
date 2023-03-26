(** Resizable array-based implementation of lists, inspired by Java. *)

type 'a t
(** The type of an arraylist whose elements have type ['a]. *)

val make : unit -> 'a t
(** [make ()] is an arraylist with initial capacity 10. *)

val size : 'a t -> int
(** [size al] is the number of elements in [al]. *)

exception OutOfBounds of { attempted : int; size : int }
(** Represents an attempt to access index [attempted] when the size is actually
    [size]. *)

val add : 'a t -> 'a -> unit
(** [add al elt] adds [elt] to the end of [al]. *)

val get : 'a t -> int -> 'a
(** [get al idx] is the element at index [idx] in arraylist [al]. Raises
    [OutOfBounds] if [idx] is out of range: [idx < 0 || idx >= size al]. *)

val set : 'a t -> int -> 'a -> unit
(** [set al idx elt] replaces the element at index [idx] in arraylist [al] with
    [elt]. Raises [OutOfBounds] if [idx] is out of range:
    [idx < 0 || idx >= size al]. *)
