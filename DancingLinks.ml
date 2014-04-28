open Core.Std
open Array


module type D =
  sig
    
    type node
    type m

    (* add node to the right of existing node *)	   
    val add_right : node -> node -> unit

    (* add node below the existing node *)
    val add_below : node -> node -> unit

    (* add a row to the matrix *)
    val add_row : node array -> bool array -> int -> unit

    (* removes a column or a row *)
    val remove_col : node -> unit
    val remove_row : node -> unit

    val cover : node -> unit

    val uncover : node -> unit

    (* Abstract type of a solution *)
    type solution

    (* what functions do we need here? *)
						 
  end;;

module DancingLinks : D =
  struct

    type node = {
      mutable up : node;
      mutable down : node;
      mutable left : node;
      mutable right : node;
      mutable header : node;
      mutable size : int;
      mutable name : string;
    }
		  
    type m = {
      headers : node;
      num_col: int;
    }


let one_by_one () =
  let rec m = {name = "head"; header = m;
	       size = 0; up = m; down = m;
	       left = m; right = m}
  in m



(* add new node to the right of existing node *)
let add_right n1 n2 =
  let tmp = n1.right in
  n1.right <- n2;
  n2.right <- tmp;
  n2.left <- n1;
  n2.right.left <- n2

(* add new node below existing node *)
let add_below n1 n2 =
  let tmp = n1.down in
  n1.down <- n2;
  n2.down <- tmp;
  n2.up <- n1;
  n2.down.up <- n2


(* Adds row after the headers in the matrix *)
let add_row headers row i =
  let rec add_rec n prev =
    if n < Array.length row then
      if row.(n) then begin
        let element = one_by_one () in
        element.size <- i;
        element.header <- headers.(n);
        element.name <- "";
        headers.(n).size <- headers.(n).size + 1;
        if n <> 0 then
          add_right prev element;
        add_below headers.(n) element;
        add_rec (n + 1) element
      end else
        add_rec (n + 1) prev
  in
  add_rec 0 (one_by_one ())


(* Returns a matrix only with the headers *)
let generate_headers ?primary size h =
  let headers = Array.init size (fun _ -> one_by_one ()) in
  let primary = match primary with
    | None -> size
    | Some p -> if p < 0 || p > size then invalid_arg "create"; p
  in
  headers.(0).size <- 0;
  headers.(0).name <- "C0";
  add_right h headers.(0);
  for n = 1 to primary - 1 do
    headers.(n).size <- 0;
    headers.(n).name <- String.concat ~sep:"" ["C";string_of_int n];
    add_right headers.(n - 1) headers.(n);
  done;
  headers


(* Applies f to elements of the matrix, from up to down*)
let iter_down ?(self = true) f n =
  if self then f n;
  let rec rec_iter_down node =
    if node <> n then begin
      f node;
      rec_iter_down node.down
    end
  in
  rec_iter_down n.down

(* Applies f to elements of the matrix, from right to left *)
let iter_left ?(self = true) f n =
  if self then f n;
  let rec rec_iter_left node =
    if node <> n then begin
      f node;
      rec_iter_left node.left
    end
  in
  rec_iter_left n.left

(* Applies f to elements of the matrix, from right to left *)
let iter_right ?(self = true) f n =
  if self then f n;
  let rec rec_iter_right node =
    if node <> n then begin
      f node;
      rec_iter_right node.right
    end
  in
  rec_iter_right n.right

(* Applies f to elements of the matrix, from down to up *)
let iter_up ?(self = true) f n =
  if self then f n;
  let rec rec_iter_up node =
    if node <> n then begin
      f node;
      rec_iter_up node.up
    end
  in
  rec_iter_up n.up


(* removes a column *)
let remove_col header = 
   header.right.left <- header.left;
   header.left.right <- header.right

(* removes a row *)
let remove_row row = 
   let cover_node n =
    n.down.up <- n.up;
    n.up.down <- n.down;
    n.header.size <- n.header.size - 1
  in
  iter_right ~self:false cover_node row 

(* Removes the given column and all rows that has a 1 in the spot of this column from
 the matrix *)
let cover column_header =
  remove_col column_header;
  iter_down ~self:false remove_row column_header

(* Un-removes the given column and all rows from the matrix *)
let uncover column_header =
  let uncover_node n =
    n.header.size <- n.header.size + 1;
    n.down.up <- n;
    n.up.down <- n
  in
  let uncover_row n =
    iter_left ~self:false uncover_node n
  in
  iter_up ~self:false uncover_row column_header;
  column_header.right.left <- column_header;
  column_header.left.right <- column_header


type solution = node array
end
