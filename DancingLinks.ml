
type node = {
  mutable up : node;
  mutable down : node;
  mutable left : node;
  mutable right : node;
  mutable header : node;
  mutable size : int;
  mutable name : string;
}

type m = 

let one_by_one () =
  let rec m = {name = "head"; header = m;
	       value = 0; up = m; down = m;
	       left = m; right = m}
  in m

val create ?primary: int -> bool array array -> t

val create_sparse

type solution

val list_of_solution

val get_first_solution

val count_solutions
