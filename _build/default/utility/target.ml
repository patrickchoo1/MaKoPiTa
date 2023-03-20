(** Implementation of target.mli *)

(** open Yojson.Basic.Util *)

type position = int * int
type target = { centre : position; box : int; hit : bool }

(** let load t =
  {
    centre = t |> member "center";
    box = t |> member "hit_box";
    hit = t |> member "is_hit";
  } *)

let center t = t.centre
let hit_box t = t.box
let is_hit t = t.hit
