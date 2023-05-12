open Raylib
open Engine
open Entities
open Component
open Systems
open Utility.Timer

(**********************************************************************
 * Helper function below
 **********************************************************************)

let rec print_list lst =
  match lst with
  | [] -> ()
  | x :: xs ->
      print_int x;
      print_string " ";
      print_list xs

let target_num = ref ~-1

let make_target shape time =
  target_num := !target_num + 1;
  Entities.id_of_name ("Target" ^ string_of_int !target_num)
  |> Shape.set shape |> Colors.set Color.black |> In_n_Out.set Out
  |> Timing.set time |> ignore

let make_polygon (vert_list : (int * int) list) : Shape.s =
  let rec aux (acc : Vector.s list) (vert_list : (int * int) list) : Shape.s =
    match vert_list with
    | (x, y) :: t ->
        aux ({ vec = (float_of_int x, float_of_int y, 0.0) } :: acc) t
    | [] -> Polygon { verticies = acc }
  in
  aux [] vert_list

let make_circle (r : float) (c : float * float) : Shape.s =
  Circle { radius = r; center = { vec = (fst c, snd c, 0.0) } }

(**********************************************************************
 * Level Module
 **********************************************************************)

module type LevelData = sig
  val init_targets : unit -> unit
  val music_path : string
  val target_interval : float
end

module type Level = sig
  val setup : unit -> unit
  val init : unit -> unit
  val loop : unit -> unit
end

module MakeLevel (L : LevelData) : Level = struct
  let target_timings = ref []
  let unwrap a = match a with Some a -> a | None -> failwith "None"

  let setup () =
    Raylib.init_window 800 450 "Test Game";
    Raylib.set_target_fps 30;
    Raylib.init_audio_device ()

  let compare_timings (id1 : id) (id2 : id) =
    match (Timing.get_opt id1, Timing.get_opt id2) with
    | Some t1, Some t2 -> compare t1 t2
    | _ -> failwith "No timings provided"

  let init () =
    L.init_targets ();
    (* Reserved names "Score", "Health", "Music" *)
    Entities.id_of_name "Score" |> Score.set 0 |> Entities.set_active |> ignore;
    (* Entities.id_of_name "Health"
       |> Sprite.set (Sprite.load "./assets/Skewed Rectangle.png")
       |> Health.set 10
       |> Multiposition.set [ { x = 200; y = 200 } ]
       |> ignore; *)
    Entities.id_of_name "Music"
    |> Audio.set (Audio.load L.music_path)
    |> Entities.set_active |> ignore;
    target_timings := List.sort compare_timings (Timing.get_keys ());
    Raylib.play_music_stream
      (Entities.id_of_name "Music" |> Audio.get_opt |> unwrap);
    Timer.init_timer L.target_interval

  let rec loop () =
    match Raylib.window_should_close () with
    | true ->
        Raylib.unload_music_stream
          (Entities.id_of_name "Music" |> Audio.get_opt |> unwrap);
        Raylib.close_audio_device ();
        Raylib.close_window ()
    | false ->
        (* print_list !target_timings; *)
        (* print_list (Entities.get_all_active ()); *)
        (* print_endline ""; *)
        print_endline (string_of_float (Timer.get_time ()));
        begin_drawing ();
        clear_background Color.raywhite;
        update_active ();
        Systems.update_all ();
        end_drawing ();
        loop ()

  and update_active () =
    let rec aux ids =
      match ids with
      | id :: t -> (
          match Timing.get_opt id with
          | Some time -> is_in_int id t time
          | None -> failwith "No timing")
      | [] -> ()
    and is_in_int id t time =
      match Timer.is_before_int time with
      | true -> ()
      | false -> (
          match Timer.is_after_int time with
          | true -> remove_from_active id
          | false ->
              set_to_active id;
              aux t)
    and remove_from_active id =
      Entities.remove_active id |> ignore;
      target_timings := match !target_timings with _ :: t -> t | [] -> []
    and set_to_active id =
      id |> In_n_Out.get_opt |> function
      | Some In_to_Out -> ()
      | _ -> Entities.set_active id |> ignore
    in
    aux !target_timings
end

module Level1Data : LevelData = struct
  let init_targets () =
    make_target (make_circle 30.0 (400.0, 225.0)) 0.009;
    make_target (make_circle 30.0 (400.0, 225.0)) 2.5;
    make_target (make_circle 30.0 (400.0, 225.0)) 3.9;
    make_target (make_circle 30.0 (100.0, 115.0)) 5.00;
    make_target (make_circle 30.0 (700.0, 115.0)) 7.00;
    make_target (make_circle 30.0 (600.0, 310.0)) 8.70;
    make_target (make_circle 30.0 (700.0, 115.0)) 10.00;
    make_target (make_circle 30.0 (100.0, 310.0)) 14.00;
    make_target (make_circle 30.0 (212.0, 335.0)) 16.50;
    make_target (make_circle 30.0 (200.0, 260.0)) 19.10;
    make_target (make_circle 30.0 (250.0, 310.0)) 20.00;
    make_target (make_circle 30.0 (340.0, 50.0)) 22.00;
    make_target (make_circle 30.0 (340.0, 50.0)) 24.80;
    make_target (make_circle 30.0 (100.0, 225.0)) 29.00;
    make_target (make_circle 30.0 (400.0, 225.00)) 30.00;
    make_target (make_circle 30.0 (700.0, 225.00)) 31.00;
    make_target (make_circle 30.0 (700.0, 380.00)) 33.00;
    make_target (make_circle 30.0 (400.0, 325.00)) 35.50;
    make_target (make_circle 30.0 (400.0, 420.00)) 37.00;
    make_target (make_circle 30.0 (50.0, 30.00)) 39.50;
    make_target (make_circle 30.0 (50.0, 225.00)) 41.00;
    make_target (make_circle 30.0 (50.0, 420.00)) 42.00;
    make_target (make_circle 30.0 (50.0, 420.00)) 49.00;
    make_target (make_circle 30.0 (400.0, 225.00)) 52.00;
    make_target (make_circle 30.0 (750.0, 410.00)) 55.00;
    make_target (make_circle 30.0 (400.0, 225.00)) 57.50;
    make_target (make_circle 30.0 (400.0, 225.00)) 60.00;
    make_target (make_circle 30.0 (400.0, 180.00)) 63.00;
    make_target (make_circle 30.0 (700.0, 400.00)) 66.00;
    make_target (make_circle 30.0 (700.0, 390.00)) 68.50;
    make_target (make_circle 30.0 (700.0, 380.00)) 71.00;
    make_target (make_circle 30.0 (700.0, 370.00)) 74.00;
    make_target (make_circle 30.0 (700.0, 40.00)) 76.80;
    make_target (make_circle 30.0 (700.0, 225.00)) 82.00;
    make_target (make_circle 30.0 (400.0, 225.00)) 85.00;
    make_target (make_circle 30.0 (100.0, 225.00)) 87.60;
    make_target (make_circle 30.0 (30.0, 30.00)) 90.00;
    make_target (make_circle 30.0 (100.0, 225.00)) 93.00;
    make_target (make_circle 30.0 (770.0, 420.00)) 95.80;
    make_target (make_circle 30.0 (770.0, 320.00)) 101.00;
    make_target (make_circle 30.0 (750.0, 320.00)) 104.00;
    make_target (make_circle 30.0 (720.0, 320.00)) 107.00;
    make_target (make_circle 30.0 (700.0, 320.00)) 111.00;
    make_target (make_circle 30.0 (680.0, 320.00)) 115.00;
    make_target (make_circle 30.0 (60.0, 400.00)) 117.50;
    make_target (make_circle 30.0 (60.0, 350.00)) 118.00;
    make_target (make_circle 30.0 (60.0, 300.00)) 118.70;
    make_target (make_circle 30.0 (400.0, 225.00)) 123.00;
    make_target (make_circle 30.0 (750.0, 410.00)) 125.50;
    make_target (make_circle 30.0 (400.0, 225.00)) 128.70;
    make_target (make_circle 30.0 (400.0, 225.00)) 131.3;
    make_target (make_circle 30.0 (400.0, 180.00)) 134.00;
    make_target (make_circle 30.0 (700.0, 400.00)) 137.00;
    make_target (make_circle 30.0 (700.0, 390.00)) 140.00;
    make_target (make_circle 30.0 (700.0, 380.00)) 142.8;
    make_target (make_circle 30.0 (30.0, 420.00)) 144.5;
    make_target (make_circle 30.0 (80.0, 380.00)) 144.8;
    make_target (make_circle 30.0 (100.0, 350.00)) 145.00;
    make_target (make_circle 30.0 (120.0, 330.00)) 147.5;
    make_target (make_circle 30.0 (140.0, 310.00)) 145.80;
    make_target (make_circle 30.0 (160.0, 290.00)) 148.00;
    make_target (make_circle 30.0 (180.0, 270.00)) 149.00;
    make_target (make_circle 30.0 (200.0, 250.00)) 152.00;
    make_target (make_circle 30.0 (200.0, 250.00)) 156.00;
    make_target (make_circle 30.0 (200.0, 260.00)) 158.50;
    make_target (make_circle 30.0 (700.0, 225.00)) 164.00;
    make_target (make_circle 30.0 (700.0, 380.00)) 167.00;
    make_target (make_circle 30.0 (770.0, 320.00)) 169.50;
    make_target (make_circle 30.0 (700.0, 380.00)) 172.50;
    make_target (make_circle 30.0 (30.0, 420.00)) 175.00;
    make_target (make_circle 30.0 (30.0, 420.00)) 178.00;
    make_target (make_circle 30.0 (80.0, 380.00)) 181.0;
    make_target (make_circle 30.0 (100.0, 350.00)) 183.5;
    make_target (make_circle 30.0 (400.0, 225.00)) 186.00;
    make_target (make_circle 30.0 (400.0, 225.00)) 189.00

  let music_path = "music/plantasia.mp3"
  let target_interval = 1.0
end

module Level1 = MakeLevel (Level1Data)
