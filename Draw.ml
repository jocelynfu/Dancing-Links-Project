open Core.Std
open Graphics
open Pentomino
open Event51

#load "graphics.cma";;
(* helper module for drawing graphics *)

exception Error of string

let ratio = 30
let b_h = 30
let b_w = 60
let boardw = ref 0 
let boardh = ref 0 
let counter = ref 1
let count_fifteen = ref 0 
let count_twelve = ref 0
let count_ten = ref 0


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

let draw_all w h counter =
  draw_board w h;
  let sol = Pentomino.solve w h counter in
  draw_solution sol (w,h)

let count_by_board w h = 
  let head = Pentomino.generate_headers w h in
  let _ = Printf.printf "1 ";flush_all () in 
  let matrix = Pentomino.create_rows w h head in
  let _ = Printf.printf "2 ";flush_all () in 
  let num_sol = Pentomino.count_solutions matrix in
  let _ = Printf.printf "%d done counting! " w; flush_all () in
  if w = 15 then count_fifteen := num_sol 
  else if w = 12 then count_twelve := num_sol 
  else if w = 10 then count_ten := num_sol 
  else ()


let erase () = 
  Graphics.set_color white; 
  Graphics.fill_rect 0 0 (Graphics.size_x ()) 
		     ((Graphics.size_y ()) - (4 * b_h))


class type button =
	  object
	    val x : int
	    val y : int
	    method draw : unit
	    method do_change : int*int -> unit
	  end

class change_sol =
object(self)
  val x = 7 * b_w 
  val y = (Graphics.size_y ()) - (2 * b_h)

  initializer 
  self#draw

  method draw = 
    Graphics.set_color black;
    Graphics.fill_rect x y (2*b_w) b_h;	
    Graphics.moveto (x+10) (y+10);
    Graphics.set_color white;
    Graphics.draw_string "Change Solution" 

  method do_change status =
      let a = status.mouse_x in
      let b = status.mouse_y in
      if a > x && a < (x+2*b_w) && b > y && b < (y+b_h)
      then (counter := !counter + 1; erase (); 
	    draw_all !boardw !boardh !counter)
      else ()
end

class four_by_fifteen =
object (self)

  val x = b_w

  val y = (Graphics.size_y ()) - (2 * b_h)

  initializer 
    self#draw

  method draw = 
    Graphics.set_color black;
    Graphics.fill_rect x y b_w b_h;	
    Graphics.moveto (x+10) (y+10);
    Graphics.set_color white;
    Graphics.draw_string "4 by 15" 

  method do_change status =
      let a = status.mouse_x in
      let b = status.mouse_y in
      if a > x && a < (x+b_w) && b > y && b < (y+b_h)
      then (boardw := 15; boardh := 4; counter := 1;
	    erase (); draw_all 15 4 1)
      else ()


end
  
class five_by_twelve =
object(self)

  val x = 3 * b_w 
  val y = (Graphics.size_y ()) - (2 * b_h)

  initializer 
  self#draw

  method draw = 
    Graphics.set_color black;
    Graphics.fill_rect x y b_w b_h;	
    Graphics.moveto (x+10) (y+10);
    Graphics.set_color white;
    Graphics.draw_string "5 by 12" 

 method do_change status =
      let a = status.mouse_x in
      let b = status.mouse_y in
      if a > x && a < (x+b_w) && b > y && b < (y+b_h)
      then (boardw := 12; boardh := 5; counter := 1; 
	    erase (); draw_all 12 5 1)
      else ()
end
  

class six_by_ten =
object(self)
  val x = 5 * b_w
  val y = (Graphics.size_y ()) - (2 * b_h)

  initializer 
  self#draw

  method draw = 
    Graphics.set_color black;
    Graphics.fill_rect x y b_w b_h;	
    Graphics.moveto (x+10) (y+10);
    Graphics.set_color white;
    Graphics.draw_string "6 by 10" 

 method do_change status =
      let a = status.mouse_x in
      let b = status.mouse_y in
      if a > x && a < (x+b_w) && b > y && b < (y+b_h)
      then (boardw := 10; boardh := 6; counter := 1; 
	    erase (); draw_all 10 6 1)
      else ()
end

class count_sol =
object(self)
  val x = 7 * b_w 
  val y = (Graphics.size_y ()) - (4 * b_h)

  initializer 
  self#draw

  method draw = 
    Graphics.set_color black;
    Graphics.fill_rect x y (2*b_w) b_h;	
    Graphics.moveto (x+10) (y+10);
    Graphics.set_color white;
    Graphics.draw_string "Count Solutions" 

  method do_change status =
      let a = status.mouse_x in
      let b = status.mouse_y in
      if a > x && a < (x+2*b_w) && b > y && b < (y+b_h)
      then 
	(let s_x = (Graphics.size_x ()/2) - 65 in
	 let s_y = 30 in 
	 Graphics.moveto s_x s_y;
	 Graphics.set_color black;
	 let s =
	   match !boardw with
	   | 15 -> "Number of Solutions: "^ (string_of_int !count_fifteen)
	   | 12 -> "Number of Solutions: "^ (string_of_int !count_twelve)
	   | 10 -> "Number of Solutions: "^ (string_of_int !count_ten)
	   | _ -> "Please select a board first!" in
	 Graphics.draw_string (s))
      else ()
end
  	      

let run () =
  Graphics.open_graph "";
  let button1 = new four_by_fifteen in
  let button2 = new five_by_twelve in
  let button3 = new six_by_ten in
  let button4 = new change_sol in
  let button5 = new count_sol in
  let _ = (count_by_board 15 4; 
  count_by_board 12 5;
  count_by_board 10 6) in
  let rec change () =
    let status = Graphics.wait_next_event [Graphics.Button_down] in
    button1#do_change status; button2#do_change status; button3#do_change status;
    button4#do_change status; button5#do_change status; change () in 
  change ();
  ignore (Graphics.read_key ());; 

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
		 ["W";"4,8";"5,8";"5,9";"6,9";"6,10"]] (10,6)*) 

run ();;
