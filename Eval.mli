module type EVAL = 
 sig
   type game
   val evaluate: bool -> game -> int
   val moreI : int
   val lessI: int
   val is_leaf: bool -> game -> bool
   val is_stable: bool -> game -> bool
   type state = W | L | N | C
   val state_of : bool -> game -> state
 end ;;