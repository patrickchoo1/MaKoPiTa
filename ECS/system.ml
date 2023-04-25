open Entities
open Component

module System = struct
  module type Sig = sig
    val update : unit -> unit
  end

  let create on_update components =
    let module S = struct
      let update () =
        on_update (Entities.get_active components);
        ()
    end in
    (module S : Sig)

  let update system =
    let module S = (val system : Sig) in
    S.update ()
end

(**********************************************************************
 * Add systems below
 **********************************************************************)

module VectorMath = struct
  let components : (module Component.Sig) list = [ (module Vector) ]
  let zero_vec : Vector.s = { vec = (0., 0., 0.) }

  let make_vec (x : float) (y : float) (z : float) : Vector.s =
    { vec = (x, y, z) }

  let add (v1 : Vector.s) (v2 : Vector.s) : Vector.s =
    match (v1.vec, v2.vec) with
    | (x1, y1, z1), (x2, y2, z2) -> { vec = (x1 +. x2, y1 +. y2, z1 +. z2) }

  let sub (v1 : Vector.s) (v2 : Vector.s) : Vector.s =
    match (v1.vec, v2.vec) with
    | (x1, y1, z1), (x2, y2, z2) -> { vec = (x1 -. x2, y1 -. y2, z1 -. z2) }

  let neg (v : Vector.s) : Vector.s =
    match v.vec with x, y, z -> { vec = (~-.1. *. x, ~-.1. *. y, ~-.1. *. z) }

  let scale (v : Vector.s) (s : float) : Vector.s =
    match v.vec with x, y, z -> { vec = (x *. s, y *. s, z *. s) }

  let dot (v1 : Vector.s) (v2 : Vector.s) : float =
    match (v1.vec, v2.vec) with
    | (x1, y1, z1), (x2, y2, z2) -> (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

  let cross (v1 : Vector.s) (v2 : Vector.s) : Vector.s =
    match (v1.vec, v2.vec) with
    | (ax, ay, az), (bx, by, bz) ->
        {
          vec =
            ( (ay *. bz) -. (az *. by),
              (az *. bx) -. (ax *. bz),
              (ax *. by) -. (ay *. bx) );
        }

  let magnitude (v : Vector.s) : float =
    match v.vec with x, y, z -> sqrt ((x ** 2.) +. (y ** 2.) +. (z ** 2.))

  let on_update ids = ()

  include (val System.create on_update components : System.Sig)
end

module ShapeCollisionDetection = struct
  include VectorMath

  let components : (module Component.Sig) list =
    [ (module Position); (module Vector); (module Shape) ]

  let support (s1 : Shape.s) (s2 : Shape.s) (d : Vector.s) : Vector.s =
    match (s1, s2) with
    | Polygon { verticies = verts1 }, Polygon { verticies = verts2 } ->
        let p1 =
          List.fold_left
            (fun acc v -> if dot v d > dot acc d then v else acc)
            (List.hd verts1) verts1
        in
        let p2 =
          List.fold_left
            (fun acc v -> if dot v (neg d) > dot acc (neg d) then v else acc)
            (List.hd verts2) verts2
        in
        sub p1 p2
    | Polygon { verticies = verts1 }, Circle { radius = r2; center = c2 } ->
        let p1 =
          List.fold_left
            (fun acc v -> if dot v d > dot acc d then v else acc)
            (List.hd verts1) verts1
        in
        let p2 = add c2 (scale (scale d r2) (1. /. magnitude d)) in
        sub p1 p2
    | Circle { radius = r1; center = c1 }, Polygon { verticies = verts2 } ->
        let p1 = add c1 (scale (scale d r1) (1. /. magnitude d)) in
        let p2 =
          List.fold_left
            (fun acc v -> if dot v (neg d) > dot acc (neg d) then v else acc)
            (List.hd verts2) verts2
        in
        sub p1 p2
    | Circle { radius = r1; center = c1 }, Circle { radius = r2; center = c2 }
      ->
        let p1 = add c1 (scale (scale d r1) (1. /. magnitude d)) in
        let p2 = add c2 (scale (scale d r2) (1. /. magnitude d)) in
        sub p1 p2

  let line_case simplex d =
    match !simplex with
    | [ b; a ] ->
        let ab, ao = (sub b a, neg a) in
        let ab_perp = cross ab ao |> cross ab in
        d := ab_perp;
        if ab_perp = zero_vec then true else false
    | _ -> failwith "Invalid Line Simplex"

  let triangle_case simplex d =
    match !simplex with
    | [ c; b; a ] ->
        let ab, ac, ao = (sub b a, sub c a, neg a) in
        let ab_perp = cross ac ab |> cross ab in
        let ac_perp = cross ab ac |> cross ac in
        if dot ab_perp ao > 0. then (
          d := ab_perp;
          simplex := [ b; a ];
          false)
        else if dot ac_perp ao > 0. then (
          d := ac_perp;
          simplex := [ c; a ];
          false)
        else true
    | _ -> failwith "Invalid Triangle Simplex"

  let handle_simplex simplex d =
    match List.length !simplex with
    | 2 -> line_case simplex d
    | 3 -> triangle_case simplex d
    | _ -> failwith "Invalid Simplex"

  let gfk_collision (s1 : Shape.s) (s2 : Shape.s) : bool =
    let d = ref (make_vec 1. 1. 0.) in
    let simplex = ref [ support s1 s2 !d ] in
    d := sub zero_vec (List.hd !simplex);
    let rec loop simplex d =
      let a = support s1 s2 !d in
      if dot a !d < 0. then false
      else
        let simplex' = ref (a :: !simplex) in
        handle_simplex simplex' d || loop simplex' d
    in
    loop simplex d

  let on_update ids = ()

  include (val System.create on_update components : System.Sig)
end
