(* AF: Two [int array]'s [|1,2,...,n|] and [|1,2,...,n|] represents the
   sparse/dense set {1,2,...,n} {1,2,...,n} respectively. The sparse set
   contains indexes to the dense set which contain the actual values. Arrays can
   grow if elements beyond the capacity are introduced.
   RI: Each Dense and Sparse have the same capacities. Dense[Sparse[x]] = x,
   however, only applies to elements in the set. *)

type t = {
  mutable sparse : int array;
  mutable dense : int array;
  mutable capacity : int; (* Capacity of Dense/Sparse Sets *)
  mutable size : int; (* Size of values in Dense Set  *)
}

let debug = false

let rep_ok arr =
  let rec aux n =
    if n < 0 then true
    else
      let x = Array.get arr.dense n in
      match x = (x |> Array.get arr.sparse |> Array.get arr.dense) with
      | true -> aux (n - 1)
      | false -> false
  in
  aux (arr.size - 1)

let make n =
  {
    sparse = Array.init n (fun i -> i);
    dense = Array.init n (fun i -> i);
    capacity = n;
    size = 0;
  }

let search x arr =
  if debug then if rep_ok arr then () else raise (Failure "RI violated") else ();

  match x with
  | i when i > arr.capacity -> -1
  | i -> (
      try
        let index = arr.sparse.(i) in
        if index < arr.size && arr.dense.(index) = i then index else -1
      with _ -> -1)

let grow arr x =
  if debug then if rep_ok arr then () else raise (Failure "RI violated") else ();

  let rec incr_cap cap =
    let double_cap = cap * 2 in
    if x > double_cap then incr_cap double_cap else double_cap
  in

  let new_capacity = incr_cap arr.capacity in

  let copy_arr set i = if i < arr.capacity then set.(i) else i in

  let new_sparse = Array.init new_capacity (copy_arr arr.sparse) in
  let new_dense = Array.init new_capacity (copy_arr arr.dense) in
  arr.capacity <- new_capacity;
  arr.sparse <- new_sparse;
  arr.dense <- new_dense;
  ()

let insert x arr =
  if debug then if rep_ok arr then () else raise (Failure "RI violated") else ();

  match search x arr with
  | s when s != -1 -> arr
  | _ ->
      if x >= arr.capacity then grow arr x;
      Array.set arr.dense arr.size x;
      Array.set arr.sparse x arr.size;
      arr.size <- arr.size + 1;
      arr

let delete x arr =
  if debug then if rep_ok arr then () else raise (Failure "RI violated") else ();

  match search x arr with
  | s when s = -1 -> arr
  | index ->
      let temp = arr.dense.(arr.size - 1) in
      Array.set arr.dense index temp;
      Array.set arr.sparse temp index;
      arr.size <- arr.size - 1;
      arr

let clear arr =
  if debug then if rep_ok arr then () else raise (Failure "RI violated") else ();

  arr.size <- 0;
  arr

let set_to_list arr =
  if debug then if rep_ok arr then () else raise (Failure "RI violated") else ();

  let rec aux n acc =
    if n >= 0 then aux (n - 1) (arr.dense.(n) :: acc) else acc
  in
  aux (arr.size - 1) []
