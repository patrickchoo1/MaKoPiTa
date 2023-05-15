open Component

module Components = struct
  let components : (module Component.Sig) array =
    [|
      (module Position);
      (module Multiposition);
      (module Vector);
      (module Shape);
      (module In_n_Out);
      (module Colors);
      (module Score);
      (module Health);
      (module Timing);
      (module Sprite);
      (module Audio);
    |]

  let reset () = Array.iter Component.reset components
end
