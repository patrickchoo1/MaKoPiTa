(**********************************************************************
 * Timer module below
 **********************************************************************)

module Timer = struct
  let start_time = ref 0.0
  let interval = ref 0.0

  let init_timer time_int =
    interval := time_int;
    start_time := Unix.gettimeofday ()

  let get_interval () = !interval
  let get_time () = Unix.gettimeofday () -. !start_time
  let is_before_int time = get_time () <= time -. !interval
  let is_after_int time = get_time () >= time +. !interval

  let between_zero_and_one x =
    if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x

  let percent_of_int time =
    let t = (get_time () -. (time -. !interval)) /. !interval in
    between_zero_and_one t
end
