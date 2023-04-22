open Component

module type C = sig
  val reset : unit -> unit
end

module Components : C = struct
  let components : (module Component.Sig) array =
    [| (module Example1); (module Example2) |]

  let reset () = Array.iter Component.reset components
end
