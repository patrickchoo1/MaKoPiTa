open Graphics
open Target.Circle;;

open_graph " 640x480";;

try
  while true do
    let st = wait_next_event [ Mouse_motion ] in
    let x, y = center in
    let (Radius r) = hit_box in
    set_color (if is_hit (st.mouse_x, st.mouse_y) then red else yellow);
    fill_circle x y r
  done
with Exit -> ()
