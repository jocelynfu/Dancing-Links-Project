open Core.Std
open Graphics
open Pentomino

#load "graphics.cma";;
(* helper module for drawing graphics *)

exception Error of string

let ratio = 30


(* pass in initial position of board and height and width *)
let draw_board w h =
  let h = ratio * h in
  let w = ratio * w in
  let x = (Graphics.size_x ())/2 - (w/2) in
  let y = (Graphics.size_y ())/2 - (h/2) in
  Graphics.draw_rect x y w h;
  let rec grid n m =
    if n = x && m = (y + h)
    then ()
    else if n = (x + w)
    then grid x (m + ratio)
    else (Graphics.draw_rect n m ratio ratio;
    grid (n + ratio) m)
  in
  grid x y
 

(* draws and fills in one grid, 
   given the (x,y) value of the grid, the color and board size *)
let one_grid y x color (w,h)=
  let h = ratio * h in
  let w = ratio * w in
  let xo = ((Graphics.size_x ())/2) - (w/2) in
  let yo = ((Graphics.size_y ())/2) + (h/2) in
  Graphics.set_color color;
  Graphics.fill_rect (xo + ((x-1)*ratio)) (yo - (y * ratio)) ratio ratio


(* takes *)
let split_by_comma (str: string) : (int * int) =
  match String.length str with
  | 3 -> ((int_of_string (String.sub str 0 1)), 
	  (int_of_string (String.sub str 2 1)))
  | 4 -> if str.[1] = ',' then
	   ((int_of_string (String.sub str 0 1)), 
	    (int_of_string (String.sub str 2 2)))
	 else ((int_of_string (String.sub str 0 2)),
	       (int_of_string (String.sub str 3 1)))
  | 5 -> ((int_of_string (String.sub str 0 2)),
	  (int_of_string (String.sub str 3 2)))
  | _ -> raise (Error "undefined")

let sort_solution (sol: string list) =
  let f n1 n2 =
    let len_n1 = String.length n1 in
    let len_n2 = String.length n2 in
    if len_n1 = len_n2 then 0
    else if len_n1 < len_n2 then (-1)
    else 1
  in 
  List.sort f sol

let fill_grid (sol: string list) (board: int * int) =
  let sol = sort_solution sol in
  let match_color (name : string list) =
    match List.hd name with
    | Some "X" -> Graphics.rgb 0x00 0xFF 0xFF
    | Some "I" -> Graphics.rgb 0xFF 0x00 0x00
    | Some "Z" -> Graphics.rgb 0x00 0xFF 0x00
    | Some "V" -> Graphics.rgb 0xFF 0xFF 0x00
    | Some "T" -> Graphics.rgb 0xFF 0x00 0xFF
    | Some "W" -> Graphics.rgb 0xFF 0x80 0x00
    | Some "L" -> Graphics.rgb 0x00 0x00 0xFF
    | Some "Y" -> Graphics.rgb 0x7F 0x00 0xFF
    | Some "U" -> Graphics.rgb 0x80 0x80 0x80
    | Some "P" -> Graphics.rgb 0x33 0x00 0x19
    | Some "F" -> Graphics.rgb 0xFF 0x99 0xFF
    | Some "N" -> Graphics.rgb 0x99 0x99 0xFF
    | _ -> raise (Error "undefined")
  in
  let color = match_color sol in
  let pos lst = match List.tl lst with
    | Some pos -> pos
    | None -> raise (Error "impossible") in
  let f x = (split_by_comma x) in
  let pos_int = List.map ~f:f (pos sol) in
  let draw (x,y) =
    one_grid x y color board  in
  List.iter (~f: draw) pos_int

let draw_solution (sol: string list list) (board: int * int) =
  let f (lst : string list) =
    fill_grid lst board in
  List.iter (~f:f) sol



let run () =
  Graphics.open_graph "";
  draw_board 10 6;
  let sol = Pentomino.solve 10 6 in
  draw_solution sol (10,6) 

(*
draw_solution [["P";"1,1";"1,2";"2,1";"2,2";"2,3"];
		 ["I";"1,3";"1,4";"1,5";"1,6";"1,7"];
		 ["V";"1,8";"1,9";"1,10";"2,10";"3,10"];
		 ["Y";"2,4";"2,5";"2,6";"2,7";"3,5"];
		 ["Z";"2,8";"3,8";"3,7";"3,6";"4,6"];
		 ["N";"2,9";"3,9";"4,9";"4,10";"5,10"];
		 ["L";"3,1";"4,1";"5,1";"6,1";"6,2"];
		 ["U";"3,2";"3,3";"3,4";"4,2";"4,4"];
		 ["X";"4,3";"5,2";"5,3";"5,4";"6,3"];
		 ["F";"4,5";"5,5";"5,6";"6,4";"6,5"];
		 ["T";"4,7";"5,7";"6,6";"6,7";"6,8"];
		 ["W";"4,8";"5,8";"5,9";"6,9";"6,10"]] (10,6) *)
;;

run ();;
