open Component

module type C = sig
  val reset : unit -> unit
end

module Components : C = struct
  let components : (module Component.Sig) array =
    [| (module Position); (module PolygonCollider) |]

  let reset () = Array.iter Component.reset components
end
