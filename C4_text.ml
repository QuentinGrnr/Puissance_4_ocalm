 (* Module C4_text describes a text-based interface for the game Connect Four
  that is compatible with the signature DISPLAY. *)

module C4_text = struct
   open C4_rep
   type game = C4_rep.game
   type move = C4_rep.move
 
   let print_game mat = 
     for l = C4_rep.row - 1 downto 0 do
       for c = 0 to C4_rep.col - 1 do
         match mat.(l).(c) with
           C4_rep.A     -> print_string "X " 
         | C4_rep.B     -> print_string "O "
         | C4_rep.Empty -> print_string ". "
       done; 
       print_newline ()
     done ; 
     print_newline () 
       
   let home () = print_string "C4 ...\n"
   let exit () = print_string "Bye for now ... \n"
   let question s = 
     print_string s; 
     print_string " y/n ? " ; 
     read_line() = "y" 
   let q_begin () = question "Would you like to begin?"
   let q_continue () = question "Play again?"
   let q_player () = question "Is there to be a machine player ?"
 
   let won  ()= print_string "The first player won" ; print_newline ()
   let lost () = print_string "The first player lost" ; print_newline ()
   let nil () = print_string "Stalemate" ; print_newline ()
   
   let init () = 
     print_string "X: 1st player   O: 2nd player"; 
     print_newline () ; print_newline () ;
     print_game (C4_rep.game_start ()) ; print_newline()
 
   let position b c aj j = print_game j
 
   let is_move = function '1'..'7' -> true | _ -> false
 
   exception Move of int
   let rec choice player game =
     print_string ("Choose player" ^ (if player then "1" else "2") ^ " : ") ;
     let l = C4_rep.legal_moves player game 
     in try  while true do 
       let i = read_line() 
       in ( if (String.length i > 0)  && (is_move  i.[0])
       then let c = (int_of_char i.[0]) - (int_of_char '0') 
       in if List.mem c l then raise (Move c) );
       print_string "Invalid move - try again"
     done ; 
       List.hd l
     with  Move i -> i 
     | _ -> List.hd l
 end ;;
(* module C4_text :
  sig
    type game = C4_rep.game
    and move = C4_rep.move
    val print_game : C4_rep.cell array array -> unit
    val home : unit -> unit
    val exit : unit -> unit
    val question : string -> bool
    val q_begin : unit -> bool
    val q_continue : unit -> bool
    val q_player : unit -> bool
    val won : unit -> unit
    val lost : unit -> unit
    val nil : unit -> unit
    val init : unit -> unit
    val position : 'a -> 'b -> 'c -> C4_rep.cell array array -> unit
    val is_move : char -> bool
    exception Move of int
    val choice : bool -> C4_rep.cell array array -> int
  end *)