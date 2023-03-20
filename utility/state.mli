(** Representation of dynamic target state.

    This module represents the state of a target as it is being played,
    including the target's current center, the targets that have been hit, and 
    functions that cause the state to change. *)

type state

val init_state : Target.target -> state
val current_center : Target.target -> int * int
val been_hit : Target.target -> Target.target list
val new_pos : Target.target -> Target.target
