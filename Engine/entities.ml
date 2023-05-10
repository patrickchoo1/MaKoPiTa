open Utility
open Component

type name = string
type id = int

module type E = sig
  val get_active : (module Component.Sig) list -> id list
  val id_of_name : name -> id
end

module Entities : E = struct
  let entities : (name, id) Hashtbl.t = Hashtbl.create 0
  let curr_id = ref ~-1
  let active : Sparse_arraylist.t = Sparse_arraylist.make 100

  let get_active (components : (module Component.Sig) list) =
    (* print_endline "getting active"; *)
    let rec has_component id comps =
      match comps with
      | [] -> true
      | h :: t -> (
          let module Comp = (val h : Component.Sig) in
          let x = Comp.get_opt id in
          match x with None -> false | _ -> has_component id t)
    in
    let rec is_active id acc =
      match id with
      | id :: t ->
          if has_component id components then id :: acc else is_active acc t
      | [] -> acc
    in
    is_active (Sparse_arraylist.set_to_list active) []

  let id_of_name (name : name) : id =
    match Hashtbl.find_opt entities name with
    | Some id -> id
    | None ->
        curr_id := !curr_id + 1;
        Hashtbl.replace entities name !curr_id;
        let _ = Sparse_arraylist.insert !curr_id active in
        !curr_id
end
