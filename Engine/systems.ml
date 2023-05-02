open System

module type S = sig
  val update_all : unit -> unit
end

module Systems : S = struct
  let systems : (module System.Sig) array = [||]
  let update_all () = Array.iter System.update systems
end
