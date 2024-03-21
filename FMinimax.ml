open Representation
open Eval
open Minimax

module FMinimaxO 
     (Rep : REPRESENTATION)  (Eval : EVAL with type game = Rep.game)  = 
   struct
     type game = Rep.game
     type move = Rep.move
    
    (* val max_number_list : 'a list -> 'a *)
    let max_number_list l = 
      let rec max_number_list_aux l max = 
        match l with
        | [] -> max
        | h::t -> if h > max then max_number_list_aux t h else max_number_list_aux t max
      in
      match l with
      | [] -> failwith "Empty list"
      | h::t -> max_number_list_aux t h
    
    (* val min_number_list : 'a list -> 'a *)
    let min_number_list l = 
      let rec min_number_list_aux l min = 
        match l with
        | [] -> min
        | h::t -> if h < min then min_number_list_aux t h else min_number_list_aux t min
      in
      match l with
      | [] -> failwith "Empty list"
      | h::t -> min_number_list_aux t h
      
    (* val search : int -> int list -> 'a list -> 'a *)
    (* objective : return the move corresponding to the evaluation *)
    (* il faut mieux determiner la condition d'arret *)

    let rec search v l1 l2 =
      match l1, l2 with
      | [], _ | _, [] -> failwith "Empty list"
      | hd1 :: tl1, hd2 :: tl2 ->
          if hd1 = v then
            hd2
          else
            search v tl1 tl2
    


    (* val depthFirstMinmax : int -> bool -> bool -> Rep.game -> int *)
    (* objective : return the evaluation of the game state *)
    let depthFirstMinmax depth player gameState = 
      let rec depthFirstMinmax_aux depth player gameState = 
        if depth = 0 || (Eval.is_leaf player gameState) then Eval.evaluate player gameState
        else
          let list_moves = (Rep.legal_moves player gameState) in
          let list_eval = List.map (fun x -> depthFirstMinmax_aux (depth-1) (not player) (Rep.play player x gameState)) list_moves in
          if player then max_number_list list_eval else min_number_list list_eval
      in
      depthFirstMinmax_aux depth player gameState


  (* val minimax : int -> bool -> Rep.game -> Rep.move *)
  (* objective : return the best move for the player *)
  let minimax depth player gameState = 
    let list_moves = (Rep.legal_moves player gameState) in
    let list_eval = List.map (fun x -> depthFirstMinmax depth (not player) (Rep.play player x gameState)) list_moves in
    List.iter (fun x -> print_int x; print_string " ") list_eval;
    print_newline();
    print_int (max_number_list list_eval);
    print_newline();
    search (max_number_list list_eval) list_eval list_moves
  end


(* module FMinimaxO :
  functor(Rep : REPRESENTATION) ->
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
      sig
        type game = Rep.game
        and move = Rep.move
        val min_number_list : 'a list -> 'a
        val max_number_list : 'a list -> 'a
        val search : int -> int list -> 'a list -> 'a
        val depthFirstMinmax : int -> bool -> bool -> Rep.game -> int
        val minimax : int -> bool -> Rep.game -> Rep.move
      end *)


module FMinimax = (FMinimaxO : FMINIMAX) ;;
