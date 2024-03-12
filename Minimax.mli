include Representation.REPRESENTATION
include Eval.EVAL with type game := game

module type MINIMAX = sig 
   type game
   type move
   val minimax : int -> bool -> game -> move
 end ;;


module type FMINIMAX = functor (Rep : Representation.REPRESENTATION) 
   -> functor (Eval : Eval.EVAL with type game = Rep.game) 
     -> MINIMAX with type game = Rep.game
                   and type move = Rep.move ;;
(* module type FMINIMAX =
  functor(Rep : REPRESENTATION) ->
    functor
      (Eval : sig
                type game = Rep.game
                val evaluate : bool -> game -> int
                val moreI : int
                val lessI : int
                val is_leaf : bool -> game -> bool
                val is_stable : bool -> game -> bool
                type state = | W | L | N | C
                val state_of : bool -> game -> state
              end) ->
      sig
        type game = Rep.game
        and move = Rep.move
        val minimax : int -> bool -> game -> move
      end *)
