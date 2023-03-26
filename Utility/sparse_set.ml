type 'a t = {
  mutable indexes : 'a option array;
  mutable values : 'a option array;
  mutable size : int;
  mutable maxVal : int;
  mutable capacity : int;
}

let make () = raise (Failure "Unimplemented")
let get index = raise (Failure "Unimplemented")
let set index value = raise (Failure "Unimplemented")
let remove index = raise (Failure "Unimplemented")
let add index = raise (Failure "Unimplemented")
