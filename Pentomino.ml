open Core.Std
open Array
open DancingLinks
open ExactCover
open Tiles


module game = struct
  type t = DancingLinks.DancingLinks.node

  let add_right n1 n2 = DancingsLinks.DancingLinks.add_right n1 n2
  let add_below n1 n2 = DancingLinks.DancingLinks.add_below n1 n2  


  let one_node name = 
    let m = {name = name; header = m;
	       size = 0; up = m; down = m;
	       left = m; right = m}
    in m 

  let generate_positions (width:int) (height:int) : string list =
    let rec count n m =
      if n = height && m = width 
      then (string_of_int n ^ "," ^ string_of_int m) :: []
      else if n = height 
      then (string_of_int n ^ "," ^ string_of_int m) :: count 1 (m+1) 
      else (string_of_int n ^ "," ^ string_of_int m) :: count (n+1) m in 
    count 1 1


  let generate_headers (width:int) (height:int) : t =
    let nl = List.map ~f:(fun s -> one_node s)
             ["I";"L";"N";"P";"T";"U";"V";"W";"X";"Y";"Z"] 
             @ (generate_positions width height) in
    let head = one_node "F" in
    List.fold_right ~f:(fun n1 n2 -> add_right n1 n2) ~init:head nl; 
    head

  let search 
  
  let add_row (head : t) (ls : string list) : t = 

  let make_row (l:(int*int) list) (head:t) : t =
    let piecename = 
      match l with 
      | [] -> failwith "impossible"
      | (x,y) :: _ -> 
        (match y with 
         | 1 -> "X"
         | 2 -> "I"
         | 3 -> "Z"
         | 4 -> "V"
         | 5 -> "T"
         | 6 -> "W" 
         | 7 -> "L"
         | 8 -> "Y"
         | 9 -> "U"
         | 10 -> "P"
         | 11 -> "F"
         | 12 -> "N") in     
    let positions = 
      match l with 
      | [] -> failwith "impossible"
      | _ :: tl -> tl in 
    let rec convert ls = 
      match ls with 
      | [] -> []
      | (x,y) :: tl -> let name = string_of_int x ^ "," ^ string_of_int y in
                       name :: convert tl in         
    add_row head (piecename :: convert positions)                  

  let create_rows (head : t) : t = 
    let objlist = [new Tiles.XPiece;new Tiles.IPiece;new Tiles.ZPiece;
                   new Tiles.VPiece;new Tiles.TPiece;new Tiles.WPiece;
                   new Tiles.LPiece;new Tiles.YPiece;new Tiles.UPiece;
                   new Tiles.PPiece;new Tiles.FPiece;new Tiles.NPiece] in
    let rowlists = 
      List.fold_right ~f:(fun ob l -> ob#create @ l) ~init:[]  objlist in   
    List.fold_right ~f:(fun pos n -> make_row pos n) ~init:head rowlists  

