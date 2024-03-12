open C4_rep

(* The quality of a game player depends primarily on the position evaluation
 function. Module C4_eval defines evaluate, which evaluates the value of a 
 position for the specified player. This function calls eval_bloc for the four 
 compass directions as well as the diagonals. eval_bloc then calls eval_four 
 to calculate the number of pieces in the requested line. Table value provides 
 the value of a block containing 0, 1, 2, or 3 pieces of the same color. The 
 exception Four is raised when 4 pieces are aligned.
*)
module C4_eval = struct open C4_rep type game = C4_rep.game 
   let value =
     Array.of_list [0; 2; 10; 50] 
   exception Four of int 
   exception Nil_Value 
   exception Arg_invalid 
   let lessI = -10000 
   let moreI = 10000
   let eval_four m l_dep c_dep delta_l delta_c =
     let n = ref 0 and e = ref Empty 
     and x = ref c_dep  and  y = ref l_dep 
     in try 
       for i = 1 to 4 do 
         if !y<0 || !y>=row || !x<0 || !x>=col then raise Arg_invalid ; 
         ( match m.(!y).(!x) with 
           A    -> if !e = B then raise Nil_Value ;
             incr n ;
             if !n = 4 then raise (Four moreI) ;
             e := A
         | B    -> if !e = A then raise Nil_Value ;
             incr n ;
             if !n = 4 then raise (Four lessI);
             e := B; 
         | Empty -> () ) ;
             x := !x + delta_c ;
         y := !y + delta_l  
       done ; 
       value.(!n) * (if !e=A then 1 else -1)
     with 
       Nil_Value | Arg_invalid  -> 0
           
   let eval_bloc m e cmin cmax lmin lmax dx dy = 
     for c=cmin to cmax do for l=lmin to lmax do 
       e := !e + eval_four m l c dx dy
     done done
       
   let evaluate b m = 
     try let evaluation = ref 0 
     in (* evaluation of rows *)
     eval_bloc m evaluation 0 (row-1) 0 (col-4) 0 1 ;
     (* evaluation of columns *)
     eval_bloc m evaluation 0 (col-1) 0 (row-4) 1 0 ;
     (* diagonals coming from the first line (to the right) *)
     eval_bloc m evaluation 0 (col-4) 0 (row-4) 1 1 ;
     (* diagonals coming from the first line (to the left) *)
     eval_bloc m evaluation 1 (row-4) 0 (col-4) 1 1 ;
     (* diagonals coming from the last line (to the right) *)
     eval_bloc m evaluation 3 (col-1) 0 (row-4) 1 (-1) ;
     (* diagonals coming from the last line (to the left) *)
     eval_bloc m evaluation 1 (row-4) 3 (col-1) 1 (-1) ;
     !evaluation
     with Four v -> v
         
   let is_leaf b m = let v = evaluate b m 
   in v=moreI || v=lessI || legal_moves b m = []
     
   let is_stable b j = true
           
   type state = W | L | N | C 
     
   let state_of player m =
     let v = evaluate player m 
     in if v = moreI then if player then W else L
     else if v = lessI then if player then L else W
     else if legal_moves player m = [] then N else C
 end ;;
(* module C4_eval :
  sig
    type game = C4_rep.game
    val value : int array
    exception Four of int
    exception Nil_Value
    exception Arg_invalid
    val lessI : int
    val moreI : int
    val eval_four :
      C4_rep.cell array array -> int -> int -> int -> int -> int
    val eval_bloc :
      C4_rep.cell array array ->
      int ref -> int -> int -> int -> int -> int -> int -> unit
    val evaluate : 'a -> C4_rep.cell array array -> int
    val is_leaf : 'a -> C4_rep.cell array array -> bool
    val is_stable : 'a -> 'b -> bool
    type state = | G | P | N | C
    val state_of : bool -> C4_rep.cell array array -> state
  end *)