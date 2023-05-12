open Utility
open Component

type name = string
type id = int

module type E = sig
  val get_active : (module Component.Sig) list -> id list
  val set_active : id -> id
  val remove_active : id -> id
  val id_of_name : name -> id
  val get_all_active : unit -> id list
end

module Entities : E = struct
  let entities : (name, id) Hashtbl.t = Hashtbl.create 0
  let curr_id = ref ~-1
  let active : Sparse_arraylist.t = Sparse_arraylist.make 100

  let get_active (components : (module Component.Sig) list) =
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
          if has_component id components then is_active t (id :: acc)
          else is_active t acc
      | [] -> acc
    in
    is_active (Sparse_arraylist.set_to_list active) []

  let set_active (id : id) =
    Sparse_arraylist.insert id active |> ignore;
    id

  let remove_active (id : id) =
    Sparse_arraylist.delete id active |> ignore;
    id

  let get_all_active () = Sparse_arraylist.set_to_list active

  let id_of_name (name : name) : id =
    match Hashtbl.find_opt entities name with
    | Some id -> id
    | None ->
        curr_id := !curr_id + 1;
        Hashtbl.replace entities name !curr_id;
        !curr_id
end
