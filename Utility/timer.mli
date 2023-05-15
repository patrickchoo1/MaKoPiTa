module Timer : sig
  (** [Timer] module  *)

  val init_timer : float -> unit
  (** [init_timer] initializes the start time of a game when the player first 
      opens it *)

  val get_interval : unit -> float
  (** [get_interval] is the time interval the player has to hit the target*)

  val get_time : unit -> float
  (** [get_time] provides the time that player started the current level.*)

  val is_before_int : float -> bool
  (** [is_before_int] returns true if [time] comes before the interval around 
      the target time. Returns false otherwise*)

  val is_after_int : float -> bool
  (** [is_after_int] returns true if [time] comes after the interval around 
      the target time. Returns false otherwise*)

  val percent_of_int : float -> float
  (** [percent_of_int] returns the percent of the time interval surrounding the 
      beat that has passed based on the current time [time]*)
end
