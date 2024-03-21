(* —ocamlbuild C4_mainT.native —ocamlbuild -libs graphics C4_mainG.native
 *)

open FMinimax
open FMain
open FSkeleton
open C4_rep
open C4_eval
open C4_text

module C4_skeleton = 
   FSkeleton (C4_rep) (C4_text) (C4_eval)  (FMinimax (C4_rep) (C4_eval)) ;;
(* module C4_skeleton :
  sig
    val depth : int ref
    exception Won
    exception Lost
    exception Nil
    val won : unit -> unit
    val lost : unit -> unit
    val nil : unit -> unit
    val again : unit -> bool
    val play_game : C4_text.game ref
    val exit : unit -> unit
    val home : unit -> unit
    val playH : bool -> unit -> unit
    val playM : bool -> unit -> unit
    val init : unit -> (unit -> unit) * (unit -> unit)
  end
 *)

(* Once the general game skeleton has been written, games may be played in 
   various ways. Two human players may play against each other, with the program merely
   verifying the validity of the moves; a person may play against a programmed player;
   or programs may play against each other. While this last mode might not be interesting 
   for the human, it does make it easy to run tests without having to wait for a person's 
   responses. The following game demonstrates this scenario. *)
module C4_main = FMain(C4_skeleton) ;;
(* module C4_main :
  sig
    val play_game : (unit -> 'a) * (unit -> 'b) -> unit
    val main : unit -> unit
  end
 *)

C4_main.main () ;;