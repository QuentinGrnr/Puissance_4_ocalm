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
      | [] -> failwith "empty list"
      | h::t -> max_number_list_aux t h
    
    (* val min_number_list : 'a list -> 'a *)
    let min_number_list l = 
      let rec min_number_list_aux l min = 
        match l with
        | [] -> min
        | h::t -> if h < min then min_number_list_aux t h else min_number_list_aux t min
      in
      match l with
      | [] -> failwith "empty list"
      | h::t -> min_number_list_aux t h

    (* val search : int -> int list -> 'a list -> 'a *)
    (* cherche la position i de v dans l1 et retourne la valeur l2[i]*)
    let rec search i l1 l2 = 
      match l1 with
      | [] -> failwith "empty list"
      | h::t -> if h = i then List.nth l2 i else search i t l2


    (* val depthFirstMinmax : int -> bool -> bool -> Rep.game -> int *)
    open Representation

    let depthFirstMinmax depth maximizingPlayer movingPlayer gameState = 

    (* val minimax : int -> bool -> Rep.game -> Rep.move *)
    let minimax depth player gameState  =
      let list_moves = (Rep.legal_moves player gameState) in
      List.nth list_moves 0
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
