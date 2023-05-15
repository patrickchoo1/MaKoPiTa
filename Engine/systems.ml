open System

module type S = sig
  val update_all : unit -> unit
end

module Systems : S = struct
  let systems : (module System.Sig) array =
    [|
      (module PlayAudio);
      (module RenderShape);
      (module RenderSprite);
      (module MultiRenderHealth);
      (module RenderScore);
      (module ShapeCollisionDetection);
      (module AnimateTargets);
      (module Active);
    |]

  let update_all () = Array.iter System.update systems
end
