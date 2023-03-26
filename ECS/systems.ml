open System

module type S = sig end

module Systems : S = struct
  let systems : (module System.Sig) array = [||]
  let update_all () = Array.iter System.update systems
end
