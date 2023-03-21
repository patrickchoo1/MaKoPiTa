open Target
open Yojson.Basic.Util

type t = { current_target : Target.t; level : Target.t list; score : int }

let init_state level_json =
  let level =
    level_json |> member "targets" |> to_list |> List.map init_target
  in
  match level with
  | h :: t -> { current_target = h; level = t; score = 0 }
  | [] -> raise (Failure "No targets")

let next s =
  match s.level with
  | h :: t -> { current_target = h; level = t; score = s.score + 1 }
  | [] -> raise (Failure "End of Level")

let update s pos mouse =
  if Target.is_hit s.current_target pos && mouse then next s else s

let render s = Target.render s.current_target

let rec rand_list acc n =
  match n with
  | n when n = 0 -> acc
  | _ -> rand_list (Target.rand_target () :: acc) (n - 1)

let rand_state () =
  {
    current_target = Target.rand_target ();
    level = rand_list [] 100;
    score = 0;
  }
