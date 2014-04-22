open Core.Std
open Array
open Format


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
  master : node;
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



(* Adds row after the headers in the DLM *)
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

(* Returns a DLM only with the headers *)
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

(* Creates a DLM from a boolean matrix *)
let create ?primary m =
  let h = one_by_one () in
  let nc = Array.length m.(0) in
  let headers = generate_headers ?primary nc h in
  for i = Array.length m - 1 downto 0 do
    add_row headers m.(i) i
  done;
  { master = h; num_col = nc; }

let create_sparse ?primary ~columns:nc a =
  let h = one_by_one () in
  let headers = generate_headers ?primary nc h in
  let row = Array.create nc false in
  for i = Array.length a - 1 downto 0 do
    Array.fill row 0 nc false;
    List.iter ~f:(fun c -> row.(c) <- true) a.(i);
    add_row headers row i
  done;
  { master = h; num_col = nc; }

(* Applies f to elements of the DLM, from up to down*)
let iter_down ?(self = true) f n =
  if self then f n;
  let rec rec_iter_down node =
    if node <> n then begin
      f node;
      rec_iter_down node.down
    end
  in
  rec_iter_down n.down

(* Applies f to elements of the DLM, from right to left *)
let iter_left ?(self = true) f n =
  if self then f n;
  let rec rec_iter_left node =
    if node <> n then begin
      f node;
      rec_iter_left node.left
    end
  in
  rec_iter_left n.left

(* Applies f to elements of the DLM, from right to left *)
let iter_right ?(self = true) f n =
  if self then f n;
  let rec rec_iter_right node =
    if node <> n then begin
      f node;
      rec_iter_right node.right
    end
  in
  rec_iter_right n.right

(* Applies f to elements of the DLM, from down to up *)
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

(* Removes the given column and all rows in column own list from
 the DLM*)
let cover column_header =
  remove_col column_header;
  iter_down ~self:false remove_row column_header

(* Un-removes the given column and all rows in column own list from
 the DLM*)
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



(* Returns the min column *)
let choose_min h =
  let rec rec_chose min node =
    if node = h then min
    else if node.size < min.size then
      rec_chose node node.right
    else
      rec_chose min node.right
  in
  rec_chose h.right h.right.right

(* Searches for all solutions, applying [f] on each *)
let rec search f k h o =
  if h = h.right then f (o, k)
  else
    let column = choose_min h in
    let get_down r =
      o.(k) <- r;
      iter_right ~self:false (fun j -> cover j.header) r;
      search f (k + 1) h o;
      iter_left ~self:false (fun j -> uncover j.header) r
    in
      cover column;
      iter_down ~self:false get_down column;
      uncover column

type solution = node array * int

(* Returns a solution as an int list *)
let list_of_solution (o, k) =
  let rec rec_stl l i =
    if i = k then l else rec_stl (o.(i).size :: l) (i + 1)
  in
  rec_stl [] 0


(* Applies f to all solutions returned by function search *)
let iter_solution f dlm =
  let o = Array.init dlm.num_col (fun _ -> one_by_one ()) in
  search f 0 dlm.master o

let count_solutions m =
  let r = ref 0 in
  iter_solution (fun (_, _) -> r:= !r + 1) m;
  !r

let get_solution_list m =
  let list_ref = ref [] in
  iter_solution (
    fun (o, k) -> list_ref := list_of_solution (o, k) :: !list_ref
  ) m;
  !list_ref

(* Print the given solution as an int list *)
let print_list_solution l =
  List.iter ~f:(fun e -> Format.printf "%d " e) l; Format.printf "@."

exception Solution of (node array * int)

let get_first_solution m =
  try
    iter_solution (fun s -> raise (Solution s)) m;
    raise Not_found
  with
    | Solution s -> s
