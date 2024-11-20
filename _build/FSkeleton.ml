open Representation
open Display
open Eval
open Minimax


(* FSkeleton controls the moves of each player according to the rules provided
 at the start of the section based on the nature of the players (automated or not)
 and the order of the players. It needs various parameters to represent the game, 
 game states, the evaluation function, and the a b search *)

 (* The two principal functions are playH and playM, respectively controlling the 
 move of a human player (using the function Disp.choice) and that of an automated 
 player. The function init determines the nature of the players and the sorts of 
 responses for Disp.q_player. *)
module FSkeleton 
     (Rep   : REPRESENTATION) 
     (Disp   : DISPLAY with type game = Rep.game and  type move = Rep.move) 
     (Eval  : EVAL with type game = Rep.game) 
     (Minimax : MINIMAX with type game = Rep.game and  type move = Rep.move) = 
   struct
     let depth = ref 6
     exception Won
     exception Lost
     exception Nil
     let won = Disp.won
     let lost = Disp.lost
     let nil = Disp.nil
     let again = Disp.q_continue
     let play_game = ref (Rep.game_start())
     let exit = Disp.exit
     let home = Disp.home 
 
     let playH player () = 
       let choice = Disp.choice player !play_game in
       let old_game = !play_game 
       in play_game := Rep.play player choice !play_game ;
       Disp.position player choice old_game !play_game ;
       match Eval.state_of player !play_game with
         Eval.W -> raise Lost
       | Eval.L -> raise Won
       | Eval.N -> raise Nil
       | _      -> ()
 
     let playM player () = 
       let choice = Minimax.minimax !depth player !play_game in 
       let old_game = !play_game 
       in play_game := Rep.play player choice !play_game ;
       Disp.position player choice old_game !play_game ;
       match Eval.state_of player !play_game with
         Eval.W -> raise Won
       | Eval.L -> raise Lost
       | Eval.N -> raise Nil
       | _      -> ()
     
     let init () =  
       let a = Disp.q_player () in
       let b = Disp.q_player() 
       in play_game := Rep.game_start () ;
       Disp.init () ; 
       match (a,b) with  
         true,true   -> playM true, playM false
       | true,false  -> playM true, playH false
       | false,true  -> playH true, playM false
       | false,false -> playH true, playH false
 end ;;
(* module FSkeleton :
  functor(Rep : REPRESENTATION) ->
    functor
      (Disp : sig
                type game = Rep.game
                and move = Rep.move
                val home : unit -> unit
                val exit : unit -> unit
                val won : unit -> unit
                val lost : unit -> unit
                val nil : unit -> unit
                val init : unit -> unit
                val position : bool -> move -> game -> game -> unit
                val choice : bool -> game -> move
                val q_player : unit -> bool
                val q_begin : unit -> bool
                val q_continue : unit -> bool
              end) ->
      functor
        (Eval : sig
                  type game = Rep.game
                  val evaluate : bool -> game -> int
                  val moreI : int
                  val lessI : int
                  val is_leaf : bool -> game -> bool
                  val is_stable : bool -> game -> bool
                  type state = | G | P | N | C
                  val state_of : bool -> game -> state
                end) ->
        functor
          (MinMax : sig
                     type game = Rep.game
                     and move = Rep.move
                     val minmax : int -> bool -> game -> move
                   end) ->
          sig
            val depth : int ref
            exception Won
            exception Lost
            exception Nil
            val won : unit -> unit
            val lost : unit -> unit
            val nil : unit -> unit
            val again : unit -> bool
            val play_game : Disp.game ref
            val exit : unit -> unit
            val home : unit -> unit
            val playH : bool -> unit -> unit
            val playM : bool -> unit -> unit
            val init : unit -> (unit -> unit) * (unit -> unit)
          end *)
