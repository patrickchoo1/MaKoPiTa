open Raylib
open Engine
open Entities
open Component
open Components
open Systems
open System
open Utility.Timer
open Level_def

module MakeLevel (L : LevelData) : Level = struct
  let scene_id = L.level_id
  let next_scene_id = ref "End Screen"
  let unwrap a = match a with Some a -> a | None -> failwith "None"

  let setup () =
    Raylib.init_window 1200 800 "Test Game";
    Raylib.set_target_fps 60;
    Raylib.init_audio_device ()

  let compare_timings (id1 : id) (id2 : id) =
    match (Timing.get_opt id1, Timing.get_opt id2) with
    | Some t1, Some t2 -> compare t1 t2
    | _ -> failwith "No timings provided"

  let init_health () =
    let start = 25 in
    let spacing = 27 in
    Entities.id_of_name "Health"
    |> Sprite.set (Sprite.load "./assets/HealthBar.png")
    |> Health.set { curr = 10; max = 10 }
    |> Multiposition.set
         [
           { x = start; y = start };
           { x = start + (spacing * 1); y = start };
           { x = start + (spacing * 2); y = start };
           { x = start + (spacing * 3); y = start };
           { x = start + (spacing * 4); y = start };
           { x = start + (spacing * 5); y = start };
           { x = start + (spacing * 6); y = start };
           { x = start + (spacing * 7); y = start };
           { x = start + (spacing * 8); y = start };
           { x = start + (spacing * 9); y = start };
         ]
    |> Entities.set_active |> ignore

  let init_score () =
    Entities.id_of_name "Score"
    |> Score.set 0
    |> Position.set { x = 900; y = 600 }
    |> Entities.set_active |> ignore

  let init_audio () =
    Entities.id_of_name "Music"
    |> Audio.set (Audio.load L.music_path)
    |> Entities.set_active |> ignore;
    Raylib.play_music_stream
      (Entities.id_of_name "Music" |> Audio.get_opt |> unwrap)

  let init () =
    Components.reset ();
    L.init_targets ();

    (* Reserved names "Score", "Health", "Music" *)
    init_health ();
    init_score ();
    Active.init_timing (List.sort compare_timings (Timing.get_keys ()));
    init_audio ();
    Timer.init_timer L.target_interval

  let is_game_over () =
    let music = Entities.id_of_name "Music" |> Audio.get_opt |> unwrap in
    Raylib.get_music_time_length music <= Timer.get_time ()
    || (Entities.id_of_name "Health" |> Health.get_opt |> unwrap).curr <= 0

  let rec loop () =
    if Raylib.window_should_close () then (
      Raylib.close_window ();
      exit 0);

    match is_game_over () with
    | true ->
        Raylib.unload_music_stream
          (Entities.id_of_name "Music" |> Audio.get_opt |> unwrap);
        Raylib.close_audio_device ();
        Raylib.close_window ()
    | false ->
        begin_drawing ();
        clear_background Color.black;
        Systems.update_all ();
        end_drawing ();
        loop ()
end
