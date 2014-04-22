open Core.Std
open Array


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


(* Adds n2 to the right of n1 in the DLM *)
let add_right n1 n2 =
  let tmp = n1.right in
  n1.right <- n2;
  n2.right <- tmp;
  n2.left <- n1;
  n2.right.left <- n2

(* Adds n2 under n1 in the DLM *)
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
  let row = Array.make nc false in
  for i = Array.length a - 1 downto 0 do
    Array.fill row 0 nc false;
    List.iter (fun c -> row.(c) <- true) a.(i);
    add_row headers row i
  done;
  { master = h; num_col = nc; }
