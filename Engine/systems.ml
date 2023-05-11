open System

module type S = sig
  val update_all : unit -> unit
end

module Systems : S = struct
  let systems : (module System.Sig) array =
    [|
      (module RenderShape);
      (module ShapeCollisionDetection);
      (module RenderSprite);
    |]

  let update_all () = Array.iter System.update systems
end
