type 'a t = { mutable data : 'a option array; mutable size : int }
(* RI: Let [capacity] be [Array.length data]. It is required that:

    - [0 <= size <= capacity].
    - The first [size] elements of [data] are [Some _], and the remaining
      [capacity - size] elements are [None].

    AF: If [data] is [[|Some e0; Some e1; ...; Some en; None; ...; None|]], it
    represents the list [[e0; e1; ... en]]. *)

let make () = { data = Array.make 100 None; size = 0 }
let size al = al.size

let grow al =
  (* allocate a new array of twice the old capacity *)
  let new_capacity = Array.length al.data * 2 in
  let new_data = Array.make new_capacity None in
  (* copy all the old elements into the new array *)
  Array.blit al.data 0 new_data 0 al.size;
  (* mutate the arraylist to store the new array *)
  al.data <- new_data

let add al elt =
  if al.size = Array.length al.data then grow al;
  al.data.(al.size) <- Some elt;
  al.size <- al.size + 1

exception OutOfBounds of { attempted : int; size : int }

let check_bounds al idx =
  if not (0 <= idx && idx < al.size) then
    raise (OutOfBounds { attempted = idx; size = al.size })
  else ()

let get al idx =
  check_bounds al idx;
  match al.data.(idx) with
  | Some elt -> elt
  | None -> raise (Failure "RI violated") [@coverage off]

let set al idx elt =
  check_bounds al idx;
  al.data.(idx) <- Some elt
