open Yojson.Basic.Util
open Raylib

type shape = Circle of float
type position = int * int
type t = { id : int; shape : shape; center : position }

let init_target target_json =
  {
    id = target_json |> member "id" |> to_string |> int_of_string;
    shape =
      (let s = target_json |> member "shape" |> to_list |> List.map to_string in
       match s with
       | [ e1; e2 ] when e1 = "circle" -> Circle (float_of_string e2)
       | _ -> raise (Failure "Invalid shape"));
    center =
      (let c =
         target_json |> member "center" |> to_list |> List.map to_string
         |> List.map int_of_string
       in
       match c with
       | [ e1; e2 ] -> (e1, e2)
       | _ -> raise (Failure "Invalid center"));
  }

let get_id target = target.id

let is_hit_circle target pos r =
  let x1, y1 = pos in
  let x2, y2 = target.center in
  sqrt (float_of_int (((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2)))) <= r

let is_hit target pos =
  match target.shape with Circle r -> is_hit_circle target pos r

let render target =
  let x, y = target.center in
  match target.shape with Circle r -> Raylib.draw_circle x y r Color.black

let rand_target () =
  {
    id = -1;
    shape = Circle (Random.float 50. +. 100.0);
    center = (Random.int 1000, Random.int 1000);
  }
