(** Representation of static target data.

    This module represents the data stored in targets, including the center 
    position [center], the edge positions [hit_box], and the hit-state [is_hit]. 
    
    Note: a valid target is confined within the game screen. *)

type position = int * int
(** The type [position] represents the (x,y) coordinates on the game screen. *)

type target
(** The abstract type of values representing target. *)

(** val load : Yojson.Basic.t -> target *)

val center : target -> position
(** [center t] is the center of target [t]. Requires: [t] is a valid target 
    representation. *)

val hit_box : target -> int
(** [hit_box t] is the hit box of target [t]. Requires: [t] is a valid target
    representation. *)

val is_hit : target -> bool
(** [is_hit t] is the hit-state of target [t]. If Requires: [t] is a valid
    target representation. *)
