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
