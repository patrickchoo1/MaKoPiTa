open Entities
open Component

module System = struct
  module type Sig = sig
    val update : unit -> unit
  end

  let create on_update components =
    let module S = struct
      let update () =
        on_update (Entities.get_active components);
        ()
    end in
    (module S : Sig)

  let update system =
    let module S = (val system : Sig) in
    S.update ()
end

(**********************************************************************
 * Add systems below
 **********************************************************************)

module CollisionDetection = struct
  let components : (module Component.Sig) list =
    [ (module Position); (module Shape) ]

  let on_update ids = ()

  include (val System.create on_update components : System.Sig)
end
