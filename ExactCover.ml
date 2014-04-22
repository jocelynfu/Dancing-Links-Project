open Format


type solution = int list
(** a solution is a set of rows *)

let print_boolean_array fmt a =
  Array.iter (
    fun cell ->
      if cell then fprintf fmt "1"
      else fprintf fmt "0"
  ) a

(* display a boolean matrix *)
let print_boolean_matrix fmt m =
  Array.iter (
    fun col ->
      print_boolean_array fmt col;
      fprintf fmt "@\n"
  ) m

let print_matrix_size fmt p =
  fprintf fmt "%dx%d" (Array.length p) (Array.length p.(0))


module type S = sig
  type t
  val create: ?primary:int -> bool array array -> t
  val create_sparse: ?primary:int -> columns:int -> int list array -> t
  val find_solution: t -> solution
  val iter_solution: (solution -> unit) -> t -> unit
  val count_solutions: t -> int
  module type ARITH = sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
  end
  module Count(A: ARITH) : sig
    val count_solutions: t -> A.t
  end
end


module D = struct
  type t = Dlx.t
  let create = Dlx.create
  let create_sparse = Dlx.create_sparse
  let find_solution p = Dlx.list_of_solution (Dlx.get_first_solution p)
  let iter_solution f p = Dlx.iter_solution (
    fun e -> f (Dlx.list_of_solution e)) p
  let count_solutions p = Dlx.count_solutions p
  module type ARITH = sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
  end
  module Count = functor (A: ARITH) ->
  struct
    let count_solutions p =
      let r = ref A.zero in
      iter_solution (fun _ -> r:= A.add !r A.one) p;
      !r
  end
end
