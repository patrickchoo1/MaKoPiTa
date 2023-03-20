module type Target = sig
  type position = int * int
  type hit_box_type = Radius of float

  val center : position
  val hit_box : hit_box_type
  val is_hit : position -> bool
end

module Circle : Target = struct
  type position = int * int
  type hit_box_type = Radius of float

  let center = (200, 200)
  let hit_box = Radius 100.0

  let is_hit pos =
    let x1, y1 = pos in
    let x2, y2 = center in
    let (Radius radius) = hit_box in
    sqrt (float_of_int (((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2))))
    <= radius
end
