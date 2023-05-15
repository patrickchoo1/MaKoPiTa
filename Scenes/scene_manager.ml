open Level_def
open Startscreen
open Levelselect
open Instructions
open Endscreen
open Makelevel
open Level1
open Level2

module Manager = struct
  let run (scene : (module Level)) =
    let module Scene = (val scene) in
    () |> Scene.setup |> Scene.init |> Scene.loop

  let rec next (scene : (module Level)) =
    let module Scene = (val scene) in
    let next_scene : (module Level) =
      match !Scene.next_scene_id with
      | "Start Screen" -> (module StartScreen)
      | "Level Select" -> (module LevelSelectScreen)
      | "Instructions" -> (module InstructionScreen)
      | "End Screen" -> (module EndScreen)
      | "Level1" -> (module Level1)
      | "Level2" -> (module Level2)
      | _ -> failwith "No valid next scene"
    in
    run next_scene;
    next next_scene

  let start () =
    run (module StartScreen);
    next (module StartScreen)

  let load_level (leveldata : (module LevelData)) =
    let module LD = (val leveldata) in
    let module Level = MakeLevel (LD) in
    ()
end
