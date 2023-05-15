open Raylib
open Engine
open Entities
open Component
open System

(**********************************************************************
 * Helper function below
 **********************************************************************)

let rec print_list lst =
  match lst with
  | [] -> ()
  | x :: xs ->
      print_int x;
      print_string " ";
      print_list xs

let target_num = ref ~-1

let screen_normalizer ((x, y) : float * float) : float * float =
  let screen_width = float_of_int (Raylib.get_screen_width ()) in
  let screen_height = float_of_int (Raylib.get_screen_height ()) in
  let width_ratio = 0.9 in
  let height_ratio = 0.65 in
  let min_x = (1. -. width_ratio) /. 2. *. screen_width in
  let max_x = screen_width -. min_x in
  let min_y = (1. -. height_ratio) /. 2. *. screen_height in
  let max_y = screen_height -. min_y in
  let range_x = max_x -. min_x in
  let range_y = max_y -. min_y in
  let new_x = (x /. screen_width *. range_x) +. min_x in
  let new_y = (y /. screen_height *. range_y) +. min_y in
  (new_x, new_y)

let make_target shape time =
  target_num := !target_num + 1;
  Entities.id_of_name ("Target" ^ string_of_int !target_num)
  |> Shape.set shape |> Colors.set Color.white |> In_n_Out.set Out
  |> Timing.set time |> ignore

let vec_to_floats (v : int * int) : float * float =
  match v with x, y -> (float_of_int x, float_of_int y)

let vec_to_ints (v : float * float) : int * int =
  match v with x, y -> (int_of_float x, int_of_float y)

let make_polygon (vert_list : (int * int) list) : Shape.s =
  let rec aux (acc : Vector.s list) (vert_list : (int * int) list) : Shape.s =
    let vert_list' = List.map vec_to_floats vert_list in
    let vert_list'' = List.map screen_normalizer vert_list' in
    match vert_list'' with
    | (x, y) :: t -> aux ({ vec = (x, y, 0.) } :: acc) (List.map vec_to_ints t)
    | [] -> Polygon { verticies = acc }
  in
  aux [] vert_list

let make_circle (r : float) (c : float * float) : Shape.s =
  let center = screen_normalizer c in
  match center with
  | x, y -> Circle { radius = r; center = VectorMath.make_vec x y 0. }
