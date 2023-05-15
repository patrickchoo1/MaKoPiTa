type box = {
  label : string;
  action : bool -> bool;
  x : float;
  y : float;
  w : float;
  h : float;
}

let f i = float_of_int i
let i f = int_of_float f

let centralize_rectangle_top_left w h pos =
  let screen_width = f (Raylib.get_screen_width ()) in
  let screen_height = f (Raylib.get_screen_height ()) in
  let center_x = screen_width /. 2. in
  let center_y = screen_height /. pos in
  let half_width = w /. 2. in
  let half_height = h /. 2. in
  let x = center_x -. half_width in
  let y = center_y -. half_height in
  (x, y)

let create_sub_boxes x y width height labels actions padding =
  let arr =
    Array.make (List.length labels)
      { label = ""; action = (fun x -> x); x = 0.0; y = 0.0; w = 0.0; h = 0.0 }
  in
  let n = f (List.length labels) in
  let sub_width = width -. (2. *. padding) in
  let available_height = height -. (padding *. (1. +. n)) in
  let sub_height = available_height /. n in
  let rec aux labels actions curr_y remaining =
    if remaining = 0. then arr
    else
      let label, l_t =
        match labels with h :: t -> (h, t) | [] -> failwith "No more labels"
      in
      let action, a_t =
        match actions with h :: t -> (h, t) | [] -> failwith "No more actions"
      in
      let curr_x = x +. padding in
      let sub_box : box =
        { label; action; x = curr_x; y = curr_y; w = sub_width; h = sub_height }
      in
      arr.(i (n -. remaining)) <- sub_box;
      aux l_t a_t (curr_y +. sub_height +. padding) (remaining -. 1.)
  in
  aux labels actions (y +. padding) n
