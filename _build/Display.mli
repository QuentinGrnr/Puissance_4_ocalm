module type DISPLAY = sig 
   type game 
   type move
   val home: unit -> unit
   val exit: unit -> unit
   val won: unit -> unit
   val lost: unit -> unit
   val nil: unit -> unit
   val init: unit -> unit
   val position : bool -> move -> game -> game -> unit
   val choice : bool ->  game -> move
   val q_player : unit -> bool   
   val q_begin : unit -> bool
   val q_continue : unit -> bool
 end ;;
