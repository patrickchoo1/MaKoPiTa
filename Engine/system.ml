open Entities
open Component
open Raylib

module System = struct
  module type Sig = sig
    val update : unit -> unit
  end

  let create on_update =
    let module S = struct
      let update () = on_update ()
    end in
    (module S : Sig)

  let update system =
    let module S = (val system : Sig) in
    S.update ()
end

(**********************************************************************
 * Helper function below
 **********************************************************************)

module VectorMath = struct
  let zero_vec : Vector.s = { vec = (0., 0., 0.) }

  let make_vec (x : float) (y : float) (z : float) : Vector.s =
    { vec = (x, y, z) }

  let add (v1 : Vector.s) (v2 : Vector.s) : Vector.s =
    match (v1.vec, v2.vec) with
    | (x1, y1, z1), (x2, y2, z2) -> { vec = (x1 +. x2, y1 +. y2, z1 +. z2) }

  let sub (v1 : Vector.s) (v2 : Vector.s) : Vector.s =
    match (v1.vec, v2.vec) with
    | (x1, y1, z1), (x2, y2, z2) -> { vec = (x1 -. x2, y1 -. y2, z1 -. z2) }

  let neg (v : Vector.s) : Vector.s =
    match v.vec with x, y, z -> { vec = (~-.1. *. x, ~-.1. *. y, ~-.1. *. z) }

  let scale (v : Vector.s) (s : float) : Vector.s =
    match v.vec with x, y, z -> { vec = (x *. s, y *. s, z *. s) }

  let dot (v1 : Vector.s) (v2 : Vector.s) : float =
    match (v1.vec, v2.vec) with
    | (x1, y1, z1), (x2, y2, z2) -> (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

  let cross (v1 : Vector.s) (v2 : Vector.s) : Vector.s =
    match (v1.vec, v2.vec) with
    | (ax, ay, az), (bx, by, bz) ->
        {
          vec =
            ( (ay *. bz) -. (az *. by),
              (az *. bx) -. (ax *. bz),
              (ax *. by) -. (ay *. bx) );
        }

  let magnitude (v : Vector.s) : float =
    match v.vec with x, y, z -> sqrt ((x ** 2.) +. (y ** 2.) +. (z ** 2.))
end

module GJK = struct
  include VectorMath

  let support (s1 : Shape.s) (s2 : Shape.s) (d : Vector.s) : Vector.s =
    let p1 =
      match s1 with
      | Polygon { verticies = verts } ->
          List.fold_left
            (fun acc v -> if dot v d > dot acc d then v else acc)
            (List.hd verts) verts
      | Circle { radius = r; center = c } ->
          add c (scale (scale d r) (1. /. magnitude d))
      | Point { center = c } -> c
    in
    let p2 =
      match s2 with
      | Polygon { verticies = verts } ->
          List.fold_left
            (fun acc v -> if dot v d > dot acc d then v else acc)
            (List.hd verts) verts
      | Circle { radius = r; center = c } ->
          add c (scale (scale d r) (1. /. magnitude d))
      | Point { center = c } -> c
    in
    sub p1 p2

  let line_case simplex d =
    match !simplex with
    | [ b; a ] ->
        let ab, ao = (sub b a, neg a) in
        let ab_perp = cross (cross ab ao) ab in
        d := ab_perp;
        if ab_perp = zero_vec then true else false
    | _ -> failwith "Invalid Line Simplex"

  let triangle_case simplex d =
    match !simplex with
    | [ c; b; a ] ->
        let ab, ac, ao = (sub b a, sub c a, neg a) in
        let ab_perp = cross (cross ac ab) ab in
        let ac_perp = cross (cross ab ac) ac in
        if dot ab_perp ao > 0. then (
          d := ab_perp;
          simplex := [ b; a ];
          false)
        else if dot ac_perp ao > 0. then (
          d := ac_perp;
          simplex := [ c; a ];
          false)
        else true
    | _ -> failwith "Invalid Triangle Simplex"

  let handle_simplex simplex d =
    match List.length !simplex with
    | 2 -> line_case simplex d
    | 3 -> triangle_case simplex d
    | _ -> failwith "Invalid Simplex"

  let gjk_collision (s1 : Shape.s) (s2 : Shape.s) : bool =
    let d = ref (make_vec 1. 1. 0.) in
    let simplex = ref [ support s1 s2 !d ] in
    d := sub zero_vec (List.hd !simplex);
    let rec loop simplex d =
      let a = support s1 s2 !d in
      if dot a !d < 0. then false
      else
        let simplex' = ref (a :: !simplex) in
        handle_simplex simplex' d || loop simplex' d
    in
    loop simplex d
end

let unwrap a = match a with Some a -> a | None -> failwith "None"

let rec print_list lst =
  match lst with
  | [] -> ()
  | x :: xs ->
      print_int x;
      print_string " ";
      print_list xs

(**********************************************************************
 * Add systems below
 **********************************************************************)

module ShapeCollisionDetection = struct
  include GJK

  let components : (module Component.Sig) list =
    [ (module Shape); (module In_n_Out) ]

  (* Increase the score by [x] *)
  let incr_score x =
    let score_id = Entities.id_of_name "Score" in
    match Score.get_opt score_id with
    | Some s -> Score.set (s + x) score_id |> ignore
    | None -> failwith "Uninitialized score"

  (* Detection for player collision in and out of target *)
  let in_n_out (id : id) (collided : bool) (pressed : bool) =
    (* print_endline (string_of_bool collided); *)
    let state =
      match In_n_Out.get_opt id with
      | Some s -> s
      | None -> failwith "No in n' out state"
    in
    match (state, collided, pressed) with
    | Out, false, true -> In_n_Out.set Out_to_In id
    | Out_to_In, true, true -> In_n_Out.set In id
    | In, true, true -> id
    | In, false, true ->
        In_n_Out.set In_to_Out id |> ignore;
        incr_score 1;
        Entities.remove_active id
    | In_to_Out, _, _ -> id
    | _ -> In_n_Out.set Out id

  let on_update () =
    let mouse_pos : Vector.s =
      {
        vec = (float_of_int (get_mouse_x ()), float_of_int (get_mouse_y ()), 0.);
      }
    in
    let mouse : Shape.s = Point { center = mouse_pos } in
    let rec on_update_aux (ids : id list) =
      match ids with
      | id :: t -> (
          match Shape.get_opt id with
          | Some s ->
              let _ =
                in_n_out id (gjk_collision s mouse) (is_mouse_button_down Left)
              in
              on_update_aux t
          | None -> failwith "No shape component")
      | [] -> ()
    in
    on_update_aux (Entities.get_active components)

  include (val System.create on_update : System.Sig)
end

module RenderShape = struct
  let components : (module Component.Sig) list =
    [ (module Shape); (module Colors) ]

  let draw_shape (s : Shape.s) (color : Colors.s) =
    match s with
    | Point pt -> (
        match pt.center.vec with
        | x, y, _ -> draw_pixel (int_of_float x) (int_of_float y) color)
    | Circle cir -> (
        match (cir.radius, cir.center.vec) with
        | r, (x, y, _) -> draw_circle (int_of_float x) (int_of_float y) r color)
    | Polygon poly ->
        let rec draw_polygon (verts : Vector.s list) =
          match verts with
          | v1 :: v2 :: t -> (
              match (v1.vec, v2.vec) with
              | (x1, y1, _), (x2, y2, _) ->
                  draw_line (int_of_float x1) (int_of_float y1)
                    (int_of_float x2) (int_of_float y2) color;
                  draw_polygon (v2 :: t))
          | [ v_last ] -> (
              match poly.verticies with
              | v_first :: _ -> (
                  match (v_first.vec, v_last.vec) with
                  | (x1, y1, _), (x2, y2, _) ->
                      draw_line (int_of_float x1) (int_of_float y1)
                        (int_of_float x2) (int_of_float y2) color)
              | _ -> failwith "Not a polygon")
          | _ -> ()
        in
        draw_polygon poly.verticies

  let on_update () =
    (* print_list (Entities.get_active components); *)
    let rec on_update_aux ids =
      match ids with
      | id :: t -> (
          match (Shape.get_opt id, Colors.get_opt id) with
          | Some s, Some c ->
              draw_shape s c;
              on_update_aux t
          | _ -> failwith "No shape component")
      | [] -> ()
    in
    on_update_aux (Entities.get_active components)

  include (val System.create on_update : System.Sig)
end

(* MUST LOAD SPRITE AFTER INITIALIZING WINDOW *)
module RenderSprite = struct
  let components : (module Component.Sig) list =
    [ (module Sprite); (module Position) ]

  let on_update () =
    let rec on_update_aux ids =
      match ids with
      | id :: t -> (
          match (Sprite.get_opt id, Position.get_opt id) with
          | Some spr, Some pos ->
              draw_texture spr pos.x pos.y Color.white;
              on_update_aux t
          | _ -> failwith "No sprite/pos")
      | [] -> ()
    in
    on_update_aux (Entities.get_active components)

  include (val System.create on_update : System.Sig)
end

module PlayAudio = struct
  let components : (module Component.Sig) list = [ (module Audio) ]

  let on_update () =
    let rec on_update_aux ids =
      match ids with
      | id :: t -> (
          match Audio.get_opt id with
          | Some audio ->
              Raylib.update_music_stream audio;
              on_update_aux t
          | _ -> failwith "No sprite/pos")
      | [] -> ()
    in
    on_update_aux (Entities.get_active components)

  include (val System.create on_update : System.Sig)
end

module AnimateTargets = struct
  include Utility.Timer

  let components : (module Component.Sig) list =
    [ (module Shape); (module Timing) ]

  let on_update () =
    let curr_radius radius target_time =
      let percent = Timer.percent_of_int target_time in
      radius *. if percent > 1. then 1. else percent
    in
    let rec on_update_aux ids =
      match ids with
      | id :: t -> (
          match (Shape.get_opt id, Timing.get_opt id) with
          | Some (Circle c), Some time -> (
              match c.center.vec with
              | x, y, _ ->
                  draw_circle (int_of_float x) (int_of_float y)
                    (curr_radius c.radius time)
                    Color.blue;
                  on_update_aux t)
          | _ -> failwith "No circle/time")
      | [] -> ()
    in
    on_update_aux (Entities.get_active components)

  include (val System.create on_update : System.Sig)
end
