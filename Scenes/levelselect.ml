open Raylib
open Level_def
open Raygui
open Raygui_helpers

module LevelSelectScreen : Level = struct
  let scene_id = "Level Select"
  let next_scene_id = ref ""
  let box = [| 0.0; 0.0; 500.0; 400.0 |]
  let boxes = ref [||]

  let setup () =
    Raylib.init_window 1200 800 "Level Select";
    Raylib.set_target_fps 30

  let init () =
    let pos = centralize_rectangle_top_left box.(2) box.(3) 1.5 in
    box.(0) <- fst pos;
    box.(1) <- snd pos;
    let to_l1 b =
      if b then next_scene_id := "Level1";
      b
    in
    let to_l2 b =
      if b then next_scene_id := "Level2";
      b
    in
    boxes :=
      create_sub_boxes box.(0) box.(1) box.(2) box.(3) [ "Level 1"; "Level 2" ]
        [ to_l1; to_l2 ] 10.

  let rec loop () =
    match Raylib.window_should_close () with
    | true -> Raylib.close_window ()
    | false ->
        Raygui.load_style_default ();
        begin_drawing ();
        clear_background Color.black;

        Raylib.draw_text "Level Select"
          (get_screen_width () / 4)
          100 100 Color.white;
        end_drawing ();
        Raygui.group_box
          (Rectangle.create box.(0) box.(1) box.(2) box.(3))
          "Levels";
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
        if not (b1 || b2) then loop () else ()
end
