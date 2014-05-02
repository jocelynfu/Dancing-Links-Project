open Core.Std
open Array
open DancingLinks
open Tiles


  type t = DancingLinks.node
  
  (* adds a node on the right *)
  let add_right n1 n2 =  DancingLinks.add_right n1 n2

  (* adds a node below *)
  let add_below n1 n2 = DancingLinks.add_below n1 n2  

  (* create a node with the given name *)
  let one_node name =
    let rec m = {up = m; down = m;
       	left = m; right = m; header = m;
	size = 0; name = name} in
    m
	
  (* node that represents the entire matrix *)
  let master_node = one_node "header" 
			     
  
  (* generate the list of possible positions on the game board *)
  let generate_positions (width:int) (height:int) : string list =
    let rec count n m =
      if n = height && m = width
      then (string_of_int n ^ "," ^ string_of_int m) :: []
      else if n = height
      then (string_of_int n ^ "," ^ string_of_int m) :: count 1 (m+1)
      else (string_of_int n ^ "," ^ string_of_int m) :: count (n+1) m in
    let lst = count 1 1 in
    lst

  (* generate headers with the piece name and the positions as strings *)
  let generate_headers (width:int) (height:int) : t =
    let nl = List.map ~f:(fun s -> one_node s)
         	      (["F";"I";"L";"N";"P";"T";"U";"V";"W";"X";"Y";"Z"]
         	       @ (generate_positions width height)) in
    let _ = List.fold_right ~f:(fun n1 n2 -> (add_right n2 n1); n1) 
			    ~init:master_node nl in
    master_node

  (* search the header that matches with the header name of the node *)
  let rec search_name (n : t) (head: t) : t =
    if n.name = head.name then  
      head
    else search_name n head.right

  (* add a row in the matrix *)
  let add_row (head : t) (ls : string list) : t =
    let nodes = List.map ~f:(fun x -> one_node x) ls in
    match nodes with
    | [] -> 
       failwith "impossible"
    | hd :: tl -> let node =
                    List.fold_right ~f:(fun n1 n2 -> add_right n2 n1; n1)
                                    ~init:hd tl in
                  let _ = iter_right ~self:true 
                            (fun n -> n.header <- search_name n head;
                                       	 n.header.size <- (n.header.size + 1);
                                       	 add_below n.header n)
                            node in
                  master_node


  (* add the possible positions to the matrix *)	    
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
     	  | 12 -> "N"
	  | _ -> failwith "impossible") in	 
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
	    
  (* create and add possible positions as rows in the matrix *)
  let create_rows (w : int) (h : int) (head : t) : t =
    let objlist = [new Tiles.xpiece w h;new Tiles.ipiece w h;new Tiles.zpiece w h;
               	   new Tiles.vpiece w h;new Tiles.tpiece w h;new Tiles.wpiece w h;
               	   new Tiles.lpiece w h;new Tiles.ypiece w h;new Tiles.upiece w h;
               	   new Tiles.ppiece w h;new Tiles.fpiece w h;new Tiles.npiece w h] in
    let rowlists =
      List.fold_right ~f:(fun ob l -> ob#create @ l) ~init:[]  objlist in   
    List.fold_right ~f:(fun pos n -> make_row pos n) ~init:head rowlists  
  
  (* cover a column in the matrix and all rows that has a node in the column *)		  
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

let col_ref = ref 1 

(* Searches for all solutions, applying [f] on each *)
let rec search f k h o =
  if (phys_equal h h.right) then f (o, k)
  else    
    let column = choose_min h in
    let get_down r =
      o.(k) <- r;
      iter_right ~self:false 
		 (fun j -> cover j.header) r;
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

  let f = iter_solution 
	    (fun s -> if !col_ref = 1
		      then raise (Solution s)
		      else (col_ref := !col_ref - 1; ())) in
  try f m;      
      raise Not_found
  with
  | Solution s -> s
  | Not_found -> 
     (col_ref := 1; try f m; 
			raise Not_found 
		    with 
		    | Solution s -> s)
			   
					
 

let string_of_solution s : string list list =
  let (o,k) = s in
  let rec convert o counter =  
    if counter < 0
    then []
    else let node = o.(counter) in
     	  let rec print_row n1 n =
       	   if (n1.name = n.name) then [n.name]
       	   else 
	     n.name :: (print_row n1 n.right) in
     	  (print_row node node.right) :: (convert o (counter-1)) in
  convert o (k-1)

(*let get_solution m counter = 
  let n = ref counter in 
  let solution = ref [] in 
  iter_solution (fun s ->
		 if !n = 1 then solution := string_of_solution s
		 else n := !n - 1; ()) m; 
  !solution *)


let count_solutions m= 
  let r = ref 0 in
  iter_solution (fun (_, _) -> r:= !r + 1) m;
  !r


let solve (width:int) (height:int) (counter:int) : string list list =
  col_ref := counter;
  let head = generate_headers width height in
  let matrix = create_rows width height head in
  let s = string_of_solution (get_first_solution matrix) in 
  s


 

