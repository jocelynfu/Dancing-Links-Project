open Core.Std

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
  num_col : int; (* number of columns *)
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

(* add ne node below existing node *)
let add_below n1 n2 =
  let tmp = n1.down in
  n1.down <- n2;
  n2.down <- tmp;
  n2.up <- n1;
  n2.down.up <- n2


(* add row beneath the array of headers *)
(* the row is an array of boolean values *)
let add_row headers row i =
  let rec addi_rec n previous =
    if n < Array.length row then
      if row.(n) then 
	begin
	  let elt = one_by_one () in
	  elt.header <- headers.(n);
	  elt.size <- i;
	  elt.name <- headers.(n).name;
	  headers.(n).size <- headers.(n).size + 1;
	  if n <> 0 then
	    add_right previous elt;
	  add_below headers.(n) elt;
	  addi_rec (n+1) elt
	end
      else addi_rec (n+1) previous
  in
  addi_rec 0 (one_by_one ())

