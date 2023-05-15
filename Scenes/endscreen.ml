open Level_def
open Raylib
open Engine
open Entities
open Component
open Raygui_helpers
open Raygui

module EndScreen : Level = struct
  (** [from_json j] is the adventure that [j] represents. Requires: [j] is a valid
    JSON adventure representation. *)
  let scene_id = "End Screen"

  let next_scene_id = ref "Start Screen"
  let msg = ref ""
  let win_msg = "Congrats! You Won!"
  let lose_msg = "Fail! You Died!"
  let final_score = ref 0
  let home = [| 0.; 0.; 500.; 400. |]
  let boxes = ref [||]

  let setup () =
    Raylib.init_window 1200 800 "Ending Screen";
    Raylib.set_target_fps 30

  let unwrap a = match a with Some a -> a | None -> failwith "None"

  let init () =
    let fh = Entities.id_of_name "Health" |> Health.get_opt |> unwrap in
    if fh.curr > 0 then msg := win_msg else msg := lose_msg;
    final_score := Entities.id_of_name "Score" |> Score.get_opt |> unwrap;
    let pos = centralize_rectangle_top_left home.(2) home.(3) 1.5 in
    home.(0) <- fst pos;
    home.(1) <- snd pos;
    boxes :=
      create_sub_boxes home.(0) home.(1) home.(2) home.(3) [ "Main Menu" ]
        [ (fun b -> b) ]
        10.

  let rec loop () =
    match Raylib.window_should_close () with
    | true ->
        Raylib.close_window ();
        exit 0
    | false ->
        Raygui.load_style_default ();
        begin_drawing ();
        clear_background Color.black;

        Raylib.draw_text !msg
          ((get_screen_width () / 3) - 50)
          100 50 Color.white;
        end_drawing ();
        Raygui.group_box
          (Rectangle.create home.(0) home.(1) home.(2) home.(3))
          "Back to";
        Raygui.set_state ControlState.Normal;
        let b1 =
          Raygui.button
            (Rectangle.create !boxes.(0).x !boxes.(0).y !boxes.(0).w
               !boxes.(0).h)
            !boxes.(0).label
          |> !boxes.(0).action
        in
        if not b1 then loop () else ()
end
