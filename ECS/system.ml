(* open Entities *)

module System = struct
  module type Sig = sig
    val update : unit -> unit
  end

  let update system =
    let module S = (val system : Sig) in
    S.update ()
end

(**********************************************************************
 * Add systems below
 **********************************************************************)
