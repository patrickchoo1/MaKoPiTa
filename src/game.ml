open Raylib
open Engine
open Entities
open Component
open Systems

(**********************************************************************
 * Target creation helper function below
 **********************************************************************)
let target_num = ref ~-1

let make_target shape time =
  target_num := !target_num + 1;
  Entities.id_of_name ("Target" ^ string_of_int !target_num)
  |> Shape.set shape |> Colors.set Color.black |> In_n_Out.set Out_to_In
  |> Timing.set time |> ignore

let make_polygon (vert_list : (float * float) list) : Shape.s =
  let rec aux (acc : Vector.s list) (vert_list : (float * float) list) : Shape.s
      =
    match vert_list with
    | (x, y) :: t -> aux ({ vec = (x, y, 0.0) } :: acc) t
    | [] -> Polygon { verticies = acc }
  in
  aux [] vert_list

let make_circle (r : float) (c : float * float) : Shape.s =
  Circle { radius = r; center = { vec = (fst c, snd c, 0.0) } }

(**********************************************************************
 * Level Module
 **********************************************************************)

module type Targets = sig
  val init_targets : unit -> unit
end

module type Level = sig
  val setup : unit -> unit
  val init : unit -> unit
  val loop : unit -> unit
end

module MakeLevel (T : Targets) : Level = struct
  let starttime = ref 0.
  let target_timings = ref []
  let target_interval = 0.2

  let setup () =
    Raylib.init_window 800 450 "Test Game";
    Raylib.set_target_fps 60

  let compare_timings (id1 : id) (id2 : id) =
    match (Timing.get_opt id1, Timing.get_opt id2) with
    | Some t1, Some t2 -> compare t1 t2
    | _ -> failwith "No timings provided"

  let init () =
    T.init_targets ();
    (* Reserved names "Score", "Health" *)
    Entities.id_of_name "Score" |> Score.set 0 |> ignore;
    Entities.id_of_name "Health"
    |> Sprite.set (Sprite.load "./assets/Skewed Rectangle.png")
    |> Health.set 10
    |> Multiposition.set [ { x = 200; y = 200 } ]
    |> ignore;
    target_timings := List.sort compare_timings (Timing.get_keys ());
    starttime := Sys.time ()

  let rec loop () =
    match Raylib.window_should_close () with
    | true -> Raylib.close_window ()
    | false ->
        begin_drawing ();
        clear_background Color.raywhite;
        update_active !starttime !target_timings;
        Systems.update_all ();
        end_drawing ();
        loop ()

  and update_active starttime target_timings =
    let rec aux ids =
      match ids with
      | id :: t -> (
          match Timing.get_opt id with
          | Some time -> (
              let adjusted_time = time +. starttime in
              match
                Sys.time () > adjusted_time -. target_interval
                && Sys.time () < adjusted_time +. target_interval
              with
              | true ->
                  (* WIP. Add mechanism to pop off id and stuff *)
                  Entities.set_active id;
                  aux t
              | false -> Entities.remove_active id)
          | None -> aux t)
      | [] -> ()
    in
    aux target_timings
end
