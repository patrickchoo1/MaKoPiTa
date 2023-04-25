open Component

module type C = sig
  val reset : unit -> unit
end

module Components : C = struct
  let components : (module Component.Sig) array =
    [| (module Position); (module Vector); (module Shape) |]

  let reset () = Array.iter Component.reset components
end
