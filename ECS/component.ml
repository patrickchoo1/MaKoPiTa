type id = int

module Component = struct
  module type Sig = sig
    type t

    val table : (id, t) Hashtbl.t
    val set : id -> t -> unit
    val get : id -> t
  end

  let create (type s) () =
    let module C = struct
      type t = s

      let table : (id, s) Hashtbl.t = Hashtbl.create 0
      let set id value = Hashtbl.replace table id value

      let get id =
        match Hashtbl.find_opt table id with
        | Some a -> a
        | None -> raise (Failure (string_of_int id ^ " Not Found"))
    end in
    (module C : Sig with type t = s)

  let reset component =
    let module C = (val component : Sig) in
    Hashtbl.reset C.table
end

(**********************************************************************
 * Add components below
 **********************************************************************)

module Example1 = struct
  type s = { health : int }

  include (val Component.create () : Component.Sig with type t = s)
end

module Example2 = struct
  type s = { test : int }

  include (val Component.create () : Component.Sig with type t = s)
end
