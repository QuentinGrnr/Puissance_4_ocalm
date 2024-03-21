open FMain
open FMinimax
open FSkeleton
open C4_rep
open C4_eval
open C4_graph


module C4_skeletonG = 
   FSkeleton (C4_rep) (C4_graph) (C4_eval) (FMinimax (C4_rep) (C4_eval)) ;;


(* Definition of a graphical interface to test the program *)
module C4_mainG = FMain(C4_skeletonG);;
(* module C4_mainG :
  sig
    val play_game : (unit -> 'a) * (unit -> 'b) -> unit
    val main : unit -> unit
  end *)

  C4_mainG.main () ;;