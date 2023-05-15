module type LevelData = sig
  val level_id : string
  val init_targets : unit -> unit
  val music_path : string
  val target_interval : float
end

module type Level = sig
  val scene_id : string
  val next_scene_id : string ref
  val setup : unit -> unit
  val init : unit -> unit
  val loop : unit -> unit
end
