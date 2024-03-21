open Skeleton
module FMain (P : SKELETON) = 
   struct
     let play_game movements = while true do (fst movements) () ; 
       (snd movements) () done
 
     let main () = let finished = ref false 
     in P.home ();
     while not !finished do
       ( try  play_game (P.init ())
       with P.Won -> P.won ()
       | P.Lost  -> P.lost ()
       | P.Nil   -> P.nil () );
       finished := not (P.again ())
     done ;
     P.exit ()
 end ;;
(* module FMain :
  functor(P : SKELETON) ->
    sig
      val play_game : (unit -> 'a) * (unit -> 'b) -> unit
      val main : unit -> unit
    end *)
