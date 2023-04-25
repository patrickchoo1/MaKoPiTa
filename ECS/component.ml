type id = int

module Component = struct
  module type Sig = sig
    type t

    val table : (id, t) Hashtbl.t
    val set : id -> t -> id
    val get_opt : id -> t option
  end

  let create (type s) () =
    let module C = struct
      type t = s

      let table : (id, s) Hashtbl.t = Hashtbl.create 0

      let set id value =
        Hashtbl.replace table id value;
        id

      let get_opt id = Hashtbl.find_opt table id
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
  type s = { x : float; y : float }

  include (val Component.create () : Component.Sig with type t = s)
end

module Vector = struct
  type s = { vec : float * float * float }

  include (val Component.create () : Component.Sig with type t = s)
end

module Shape = struct
  type s =
    | Circle of { radius : float; center : Vector.s }
    | Polygon of { verticies : Vector.s list }

  include (val Component.create () : Component.Sig with type t = s)
end
