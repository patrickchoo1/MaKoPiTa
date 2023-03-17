module type State = sig
  type target_loc = int * int

  val score : int
  val lives : int
  val loc_lst : target_loc list
end

module Play : State = struct
  type target_loc = int * int

  let score = 1
  let lives = 1
  let loc_lst = [ (1, 1) ]
end
