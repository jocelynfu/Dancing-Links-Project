open Core.Std
open Array
open DancingLinks
open Tiles


  type t = DancingLinks.node

  let add_right n1 n2 =  DancingLinks.add_right n1 n2
  let add_below n1 n2 = DancingLinks.add_below n1 n2  


  let one_node name =
    let rec m = {up = m; down = m;
       	left = m; right = m; header = m;
	size = 0; name = name} in
    m
	

  let master_node = one_node "header" 

  
let generate_positions (width:int) (height:int) : string list =
	let rec count n m =
  	if n = height && m = width
  	then (string_of_int n ^ "," ^ string_of_int m) :: []
  	else if n = height
  	then (string_of_int n ^ "," ^ string_of_int m) :: count 1 (m+1)
  	else (string_of_int n ^ "," ^ string_of_int m) :: count (n+1) m in
	let lst = count 1 1 in
	let _ = List.map 
       ~f:(fun x -> let _ = Printf.printf "%s " x;flush_all () in ()) lst in
	lst


  let generate_headers (width:int) (height:int) : t =
	let nl = List.map ~f:(fun s -> one_node s)
         	(["F";"I";"L";"N";"P";"T";"U";"V";"W";"X";"Y";"Z"]
         	@ (generate_positions width height)) in
	let _ = List.fold_right ~f:(fun n1 n2 -> (add_right n2 n1); n1) 
			~init:master_node nl in
	let _ = Printf.printf "generate_headers " in
	master_node

  let rec search_name (n : t) (head: t) : t =
    if n.name = head.name then  
      (* let _ = Printf.printf "found head! " ; flush_all () in *)
      head
    else
       (*let _ = Printf.printf "search loop! " ; flush_all () in*)
      search_name n head.right
 
  let add_row (head : t) (ls : string list) : t =
    (* let _ = List.map ~f:(fun x -> let _ = Printf.printf "%s " x;flush_all () in ()) ls in*)
    (* let _ = Printf.printf "add_row!  "; flush_all () in *)
    let nodes = List.map ~f:(fun x -> one_node x) ls in
    match nodes with
    | [] -> let _ = Printf.printf "impossible! " ; flush_all () in
       failwith "impossible"
    | hd :: tl -> let _ =
                    List.fold_right ~f:(fun n1 n2 -> add_right n2 n1; n1)
                                    ~init:hd tl in
                  let _ = List.map
                            ~f:(fun n -> n.header <- search_name n head;
                                       	 n.header.size <- (n.header.size + 1);
                                       	 add_below n n.header)
                            nodes in
                  (*let _ = add_right head.left master_node in*)
		  (* let _ = Printf.printf "end of add_row! " ; flush_all () in*)
                  master_node

  let make_row (l:(int*int) list) (head:t) : t =
    (*let _ = Printf.printf "make_row! "; flush_all () in*)
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
	    
let create_rows (w : int) (h : int) (head : t) : t =
  let _ = Printf.printf "create_row "; flush_all () in
  let objlist = [new Tiles.xpiece w h;new Tiles.ipiece w h;new Tiles.zpiece w h;
               	 new Tiles.vpiece w h;new Tiles.tpiece w h;new Tiles.wpiece w h;
               	 new Tiles.lpiece w h;new Tiles.ypiece w h;new Tiles.upiece w h;
               	 new Tiles.ppiece w h;new Tiles.fpiece w h;new Tiles.npiece w h] in
  let rowlists =
    List.fold_right ~f:(fun ob l -> ob#create @ l) ~init:[]  objlist in   
  List.fold_right ~f:(fun pos n -> make_row pos n) ~init:head rowlists  
		  
let cover h = DancingLinks.cover h
let uncover h = DancingLinks.uncover h

  (* Returns the min column *)
  let choose_min h =
	let rec choose min node =
  	if (phys_equal node h) then min
  	else if node.size < min.size
       	then choose node node.right
  	else choose min node.right
	in
	choose h.right h.right.right


 (* let rec find_solution (h : t) (counter: int) : string list list =
	let rec remove n1 n =
   	if n.right = n1
   	then (cover n.header; [n.header.name])
   	else (cover n.header; n.header.name :: remove n1 n.right) in
	if h = h.right then print solution
	else
  	let column = choose_min h in
  	let get_down col =
  	 

 counter = 12 then []
	else if head.down = head
     	then uncover head
	else (remove head.down head.down) :: find_solution head.right (counter+1)*)
 


(* Searches for all solutions, applying [f] on each *)
let rec search f k h o =
  let _ = Printf.printf "h: %s " h.right.name; flush_all () in
  if (phys_equal h h.right) then f (o, k)
  else
    let _ = Printf.printf "k: %d " k; flush_all () in
    let column = choose_min h in
    let get_down r =
      o.(k) <- r;
      iter_right ~self:false (fun j -> 
	     (let _ = Printf.printf "j: %s " j.header.name; flush_all () in
				      cover j.header)) r;
      search f (k + 1) h o;
      iter_left ~self:false (fun j -> uncover j.header) r
    in
    cover column;
    iter_down ~self:false get_down column;
    uncover column


(* Applies f to all solutions returned by function search *)
let iter_solution f head =
  let o = Array.init 73 (fun _ -> one_node "") in
  search f 0 head o


exception Solution of (node array * int)

let get_first_solution m =
  try
	iter_solution (fun s -> raise (Solution s)) m;
	raise Not_found
  with
	| Solution s -> s

let string_of_solution s : string list list =
let _ = Printf.printf "string_of_solution "; flush_all () in
  let (o,k) = s in
  let rec convert o counter =  
	if counter < 0
	then []
	else let node = o.(counter) in
     	let rec print_row n1 n =
       	if n.right = n then [n.name]
       	else n.name :: print_row n1 n.right in
     	print_row node node :: convert o (counter-1) in
  convert o k

 let solve (width:int) (height:int) : string list list =
   let _ = Printf.printf "solve "; flush_all () in
   let head = generate_headers width height in
   let matrix = create_rows width height head in
   let s = get_first_solution matrix in
   string_of_solution s
