module type State = sig
  val score : int
  val lives : int
  val loc : int * int
end
