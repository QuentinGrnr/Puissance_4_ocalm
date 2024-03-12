open Representation

(* We choose for this game a matrix-based representation. Each element of the matrix 
   is either empty, or contains a player's piece. A move is numbered by the column. 
   The legal moves are the columns in which the final (top) row is not filled. *)

module C4_rep = struct
   type cell = A | B | Empty 
   type game = cell array array 
   type move = int 
   let col = 7 and row = 6 
   let game_start () = Array.make_matrix row col Empty 
 
   let legal_moves b m =   
     let l = ref [] in 
     for c = 0 to col-1 do if m.(row-1).(c) = Empty then l := (c+1) :: !l done;
     !l
       
   let augment mat c =  
     let l = ref row 
     in while !l > 0 && mat.(!l-1).(c-1) = Empty do  decr l done ; !l + 1
 
   let player_gen cp m e =
     let mj = Array.map Array.copy  m 
     in  mj.((augment mj cp)-1).(cp-1) <- e ; mj
 
   let play b cp m = if b then player_gen cp m A else player_gen cp m B
 end ;;

(* module C4_rep :
  sig
    type cell = | A | B | Empty
    and game = cell array array
    and move = int
    val col : int
    val row : int
    val game_start : unit -> cell array array
    val legal_moves : 'a -> cell array array -> int list
    val augment : cell array array -> int -> int
    val player_gen : int -> cell array array -> cell -> cell array array
    val play : bool -> int -> cell array array -> cell array array
  end *)


module C4_rep_T = (C4_rep : REPRESENTATION) ;;