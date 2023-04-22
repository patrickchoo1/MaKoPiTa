open Utility

type name = string
type id = int

module type E = sig
  val is_active : id -> bool
  val id_of_name : name -> id
end

module Entities : E = struct
  let entities : (name, id) Hashtbl.t = Hashtbl.create 0
  let curr_id = ref ~-1

  (* Maybe use sparse array using init after adding all the entities in *)
  let active : Sparse_arraylist.t = Sparse_arraylist.make 100
  let is_active (id : id) : bool = Sparse_arraylist.search id active != -1

  let id_of_name (name : name) : id =
    match Hashtbl.find_opt entities name with
    | Some id -> id
    | None ->
        curr_id := !curr_id + 1;
        Hashtbl.replace entities name !curr_id;
        let _ = Sparse_arraylist.insert !curr_id active in
        !curr_id
end
