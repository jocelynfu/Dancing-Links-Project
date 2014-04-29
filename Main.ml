open Core.Std
open Event51
open Pentomino

let gen_board w h =
  Draw.draw_board w h

let 6by10_initializer () : unit =
  ignore (gen_board () 10 6)

let 5by12_initializer () : unit =
  ignore (gen_board () 12 5)

let 4by15_initializer () : unit =
  ignore (gen_board () 15 4)

let 3by20_initializer () : unit =
  ignore (gen_board () 20 3)


(* Parse command-line arguments. Returns the appropriate initialization
  function to run and the current part. *)
let parse_args () : unit -> unit =
  let usage () = Printf.printf "please enter a supported game board" Sys.argv.(0); exit 1 in
  if Array.length Sys.argv <> 2 then usage ();
  match Sys.argv.(1) with
  | "6by10" -> 6by10_initializer
  | "5by12" -> 5by12_initializer
  | "4by15" -> 4by15_initializer
  | "3by20" -> 3by20_initializer
  | _ -> usage ()

let run () : unit =
  let initializer = parse_args () in
  UI.run_world initialize
;;

run ();;
