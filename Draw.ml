open Core.Std
open Graphics

#load "graphics.cma";;
(* helper module for drawing graphics *)

let ratio = 30

(* pass in initial position of board and height and width *)
let draw_board (x, y) h w =
  let h = ratio * h in
  let w = ratio * w in
  let x = x + ((Graphics.size_x ())/2) - (w/2) in
  let y = y + ((Graphics.size_y ())/2) - (h/2) in
  Graphics.draw_rect x y w h;
(*  Graphics.draw_rect x y 40 40;
  Graphics.draw_rect x (y + 40) 40 40 *)
  let rec grid n m =
    if n = x && m = (y + h)
    then ()
    else if n = (x + w)
    then grid x (m + ratio)
    else (Graphics.draw_rect n m ratio ratio;
    grid (n + ratio) m)
  in
  grid x y


let run () =
  Graphics.open_graph "";
  draw_board (1,1) 3 20;
