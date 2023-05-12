type id = int

module Component = struct
  module type Sig = sig
    type t

    val table : (id, t) Hashtbl.t
    val set : t -> id -> id
    val get_opt : id -> t option
    val get_keys : unit -> id list
  end

  let create (type s) () =
    let module C = struct
      type t = s

      let table : (id, s) Hashtbl.t = Hashtbl.create 0

      let set value id =
        Hashtbl.replace table id value;
        id

      let get_opt id = Hashtbl.find_opt table id
      let get_keys () = Hashtbl.fold (fun id _ acc -> id :: acc) table []
    end in
    (module C : Sig with type t = s)

  let reset component =
    let module C = (val component : Sig) in
    Hashtbl.reset C.table
end

(**********************************************************************
 * Add components below
 **********************************************************************)

module Position = struct
  type s = { x : int; y : int }

  include (val Component.create () : Component.Sig with type t = s)
end

module Multiposition = struct
  type pos = { x : int; y : int }
  type s = pos list

  include (val Component.create () : Component.Sig with type t = s)
end

module Vector = struct
  type s = { vec : float * float * float }

  include (val Component.create () : Component.Sig with type t = s)
end

module Shape = struct
  type s =
    | Point of { center : Vector.s }
    | Circle of { radius : float; center : Vector.s }
    | Polygon of { verticies : Vector.s list }

  include (val Component.create () : Component.Sig with type t = s)
end

module In_n_Out = struct
  type s = Out | Out_to_In | In | In_to_Out

  include (val Component.create () : Component.Sig with type t = s)
end

module Colors = struct
  type s = Raylib.Color.t

  include (val Component.create () : Component.Sig with type t = s)
end

module Score = struct
  type s = int

  include (val Component.create () : Component.Sig with type t = s)
end

module Health = struct
  type s = int

  include (val Component.create () : Component.Sig with type t = s)
end

module Timing = struct
  (* In seconds *)
  type s = float

  include (val Component.create () : Component.Sig with type t = s)
end

module Sprite = struct
  type s = Raylib.Texture2D.t

  let load path = Raylib.load_texture path

  include (val Component.create () : Component.Sig with type t = s)
end

module Audio = struct
  type s = Raylib.Music.t

  let load path = Raylib.load_music_stream path

  include (val Component.create () : Component.Sig with type t = s)
end
