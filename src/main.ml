open Target

let setup () =
  Raylib.init_window 800 450 "Rhythm Game";
  Raylib.set_target_fps 60

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    let x, y = Circle.center in
    let (Radius r) = Circle.hit_box in
    (* print_endline
       ("X: " ^ string_of_int (get_mouse_x ()) ^ ", Y:" string_of_int (get_mouse_x ())); *)
    let color =
      if Circle.is_hit (get_mouse_x (), get_mouse_y ()) then Color.black
      else Color.red
    in
    draw_circle x y r color;
    clear_background Color.raywhite;
    end_drawing ();
    loop ()

let () = setup () |> loop
