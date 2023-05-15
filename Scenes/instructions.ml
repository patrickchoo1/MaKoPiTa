open Raylib
open Level_def

module InstructionScreen : Level = struct
  let scene_id = "Instructions"
  let next_scene_id = ref "Level Select"

  let setup () =
    Raylib.init_window 800 450 "Instruction Screen";
    Raylib.set_target_fps 30

  let init () = ()

  let rec loop () =
    match Raylib.window_should_close () with
    | true ->
        Raylib.close_window ();
        exit 0
    | false ->
        let mouseX : int = Raylib.get_mouse_x () in
        let mouseY : int = Raylib.get_mouse_y () in
        if
          Raylib.is_mouse_button_pressed Raylib.MouseButton.Left
          && mouseX >= 625 && mouseX <= 775 && mouseY >= 25 && mouseY <= 65
        then Raylib.close_window ()
        else (
          begin_drawing ();
          clear_background Color.raywhite;
          Raylib.draw_rectangle 625 25 150 40 Color.blue;
          Raylib.draw_text "Level Select" 640 35 20 Color.black;
          Raylib.draw_text "How To Play:" 250 50 40 Color.black;
          Raylib.draw_text
            "Each level of this game will feature a different song, with the \
             goal\n\
             of each level being to keep up with the rhythm of that song. The \n\
             song will play at the start of each level, with a target appearing\n\
             on the first beat. The target will start out as a black circle," 50
            100 20 Color.black;
          Raylib.draw_text
            "but will turn into a blue circle as the song approaches closer \n\
             to the beat. When the circle becomes fully blue, the beat occurs \n\
             and that is the ideal time to slice through the target. To slice\n\
             through the target, hold the mouse and drag it into the circle \n\
             at any point and out of the circle at any point." 50 220 20
            Color.black;
          end_drawing ();
          loop ())
end
