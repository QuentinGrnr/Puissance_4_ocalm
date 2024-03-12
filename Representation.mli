module type REPRESENTATION = 
 sig 
   type game
   type move 
   val game_start : unit -> game
   val legal_moves: bool -> game -> move list 
   val play: bool -> move -> game -> game 
 end ;;