open Raylib
open Level_def
open Raygui
open Raygui_helpers

module StartScreen : Level = struct
  let scene_id = "Start"
  let next_scene_id = ref ""
  let menu = [| 0.; 0.; 500.; 400. |]
  let boxes = ref [||]

  let setup () =
    Raylib.init_window 1200 800 "Starting Screen";
    Raylib.set_target_fps 30

  let init () =
    let pos = centralize_rectangle_top_left menu.(2) menu.(3) 1.5 in
    menu.(0) <- fst pos;
    menu.(1) <- snd pos;

    let to_ls b =
      if b then next_scene_id := "Level Select";
      b
    in
    let to_instr b =
      if b then next_scene_id := "Instructions";
      b
    in
    boxes :=
      create_sub_boxes menu.(0) menu.(1) menu.(2) menu.(3)
        [ "Level Select"; "Instructions" ]
        [ to_ls; to_instr ] 10.

  let rec loop () =
    match Raylib.window_should_close () with
    | true ->
        Raylib.close_window ();
        exit 0
    | false ->
        begin_drawing ();
        clear_background Color.black;

        Raylib.draw_text "Pakomita"
          ((get_screen_width () / 3) - 10)
          100 100 Color.white;
        end_drawing ();
        Raygui.load_style_default ();

        Raygui.group_box
          (Rectangle.create menu.(0) menu.(1) menu.(2) menu.(3))
          "Menu";
        Raygui.set_state ControlState.Normal;

        let b1 =
          Raygui.button
            (Rectangle.create !boxes.(0).x !boxes.(0).y !boxes.(0).w
               !boxes.(0).h)
            !boxes.(0).label
          |> !boxes.(0).action
        in
        let b2 =
          Raygui.button
            (Rectangle.create !boxes.(1).x !boxes.(1).y !boxes.(1).w
               !boxes.(1).h)
            !boxes.(1).label
          |> !boxes.(1).action
        in
        if not (b1 || b2) then loop () else close_window ()
end
