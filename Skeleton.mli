module type SKELETON = sig 
   val home: unit -> unit
   val init: unit -> ((unit -> unit) * (unit -> unit))
   val again: unit -> bool
   val exit: unit -> unit
   val won: unit -> unit
   val lost: unit -> unit
   val nil: unit -> unit
   exception Won
   exception Lost
   exception Nil
 end ;;
