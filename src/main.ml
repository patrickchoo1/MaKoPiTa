(* open Engine
   open State

      let random_level = rand_state ()
      (* let test_level = init_state (Yojson.Basic.from_file "data/test.json") *)

      let setup level =
        Raylib.init_window 600 600 "Rhythm Game";
        Raylib.set_target_fps 60;
        level

      let rec loop s =
        if Raylib.window_should_close () then Raylib.close_window ()
        else
          let open Raylib in
          begin_drawing ();
          State.render s;
          (* print_endline
             ("X: " ^ string_of_int (get_mouse_x ()) ^ ", Y:" string_of_int (get_mouse_x ())); *)
          clear_background Color.raywhite;
          end_drawing ();
          loop
            (State.update s
               (get_mouse_x (), get_mouse_y ())
               (is_mouse_button_down Left))

      let () = setup random_level |> loop *)

open Raylib
open Engine
open Entities
open Component
open Systems

(* Reserved names "Score", "Health" *)
let vert_list : Vector.s list =
  [
    { vec = (5.0, 5.0, 0.0) };
    { vec = (100.0, 5.0, 0.0) };
    { vec = (700.0, 300.0, 0.0) };
    { vec = (300.0, 500.0, 0.0) };
    { vec = (5.0, 100.0, 0.0) };
  ]

let s1 : Shape.s = Polygon { verticies = vert_list }

let _ =
  Entities.id_of_name "Target"
  |> Shape.set s1 |> Colors.set Color.black |> In_n_Out.set Out_to_In |> ignore

let _ = Entities.id_of_name "Score" |> Score.set 0

let setup () =
  Raylib.init_window 800 450 "Test Game";
  Raylib.set_target_fps 60;
  Entities.id_of_name "Health"
  |> Sprite.set (Sprite.load "./assets/Skewed Rectangle.png")
  |> Position.set { x = 200; y = 200 }
  |> ignore

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else begin_drawing ();
  clear_background Color.raywhite;
  Systems.update_all ();
  print_endline
    (string_of_int
       (match Entities.id_of_name "Score" |> Score.get_opt with
       | Some v -> v
       | None -> failwith "No score"));
  end_drawing ();
  loop ()

let () = setup () |> loop
