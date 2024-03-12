open Graphics

module C4_graph = struct
   open C4_rep
   type game = C4_rep.game
   type move = C4_rep.move
   let r = 20          (* color of piece *)
   let ec = 10         (* distance between pieces *)
   let dec = 30        (* center of first piece *)
   let cote = 2*r + ec (* height of a piece looked at like a checker *)
   let htexte = 25     (* where to place text *)
   let width = C4_rep.col * cote + ec          (* width of the window *)
   let height = C4_rep.row * cote + ec + htexte (* height of the window *)
   let height_of_game = C4_rep.row * cote + ec   (* height of game space *)
   let hec = height_of_game + 7  (* line for messages *)
   let lec = 3                   (* columns for messages *)
   let margin = 4                 (* margin for buttons *)
   let xb1 = width / 2   (* position x of button1 *)
   let xb2 = xb1 + 30      (* position x of button2 *)
   let yb = hec - margin    (* position y of the buttons *)
   let wb = 25             (* width of the buttons *)
   let hb = 16             (* height of the buttons *)
 
 (* val t2e : int -> int *)
 (* Convert a matrix coordinate into a graphical coordinate *)
   let t2e i = dec + (i-1)*cote
 
 (* The Colors *)
   let cN = Graphics.black   (* trace *)
   let cA = Graphics.red     (* Human player *)
   let cB = Graphics.yellow  (* Machine player *)
   let cF = Graphics.blue    (* Game Background color *)
 (* val draw_table : unit -> unit :  Trace an empty table *)
   let draw_table () =
     Graphics.clear_graph();
     Graphics.set_color cF;
     Graphics.fill_rect 0 0 width height_of_game;
     Graphics.set_color cN;
     Graphics.moveto 0 height_of_game;
     Graphics.lineto width height_of_game;
     for l = 1 to C4_rep.row do
       for c = 1 to C4_rep.col do
         Graphics.draw_circle (t2e c) (t2e l) r
       done
     done
 
 (* val draw_piece : int -> int -> Graphics.color -> unit *)
 (* 'draw_piece l c co' draws a piece of color co at coordinates l c *)
   let draw_piece l c col =
     Graphics.set_color col;
     Graphics.fill_circle (t2e c) (t2e l) (r+1)
 
 (* val augment : Rep.item array array -> int -> Rep.move *)
 (* 'augment m c' redoes the line or drops the piece for c in m *)
   let augment mat c =
     let l = ref C4_rep.row in
     while !l > 0 && mat.(!l-1).(c-1) = C4_rep.Empty do
       decr l
     done;
     !l
 
 (* val conv : Graphics.status -> int *)
 (* convert the region where player has clicked in controlling the game *)
   let conv st =
     (st.Graphics.mouse_x - 5) / 50 + 1
 
 (* val wait_click : unit -> Graphics.status *)
 (* wait for a mouse click *)
   let wait_click () = Graphics.wait_next_event [Graphics.Button_down]
 
 (* val choiceH : Rep.game -> Rep.move *)
 (* give opportunity to the human player to choose a move *)
 (* the function offers possible moves *)
   let rec choice player  game  =
     let c = ref 0 in
     while not ( List.mem !c (C4_rep.legal_moves player game) ) do
       c := conv ( wait_click() )
     done;
     !c
 (* val home : unit -> unit : home screen *)
   let home () =
     Graphics.open_graph
       (" " ^ (string_of_int width) ^ "x" ^ (string_of_int height) ^ "+50+50");
     Graphics.moveto (height/2) (width/2);
     Graphics.set_color cF;
     Graphics.draw_string "C4";
     Graphics.set_color cN;
     Graphics.moveto 2 2;
     Graphics.draw_string "by Romuald COEFFIER & Mathieu DESPIERRE";
     ignore (wait_click ());
     Graphics.clear_graph()
 
 (* val end : unit -> unit ,  the end of the game *)
   let exit () = Graphics.close_graph()
 
 (* val draw_button : int -> int -> int -> int -> string -> unit *)
 (* 'draw_button x y w h s' draws a rectangular button at coordinates *)
 (* x,y with width w and height h and appearance s *)
   let draw_button x y w h s =
     Graphics.set_color cN;
     Graphics.moveto x y;
     Graphics.lineto x (y+h);
     Graphics.lineto (x+w) (y+h);
     Graphics.lineto (x+w) y;
     Graphics.lineto x y;
     Graphics.moveto (x+margin) (hec);
     Graphics.draw_string s
       
 (* val draw_message : string -> unit  * position message s *)
   let draw_message s =
     Graphics.set_color cN;
     Graphics.moveto lec hec;
     Graphics.draw_string s
       
 (* val erase_message : unit -> unit  erase the starting position *)
   let erase_message () =
     Graphics.set_color Graphics.white;
     Graphics.fill_rect 0 (height_of_game+1) width htexte
 
 (* val question : string -> bool *)
 (* 'question s' poses the question s, the response being obtained by *)
 (*    selecting one of two buttons, 'yes' (=true) and 'no' (=false) *)
   let question s =
     let rec attente () =
       let e = wait_click () in
       if (e.Graphics.mouse_y < (yb+hb)) && (e.Graphics.mouse_y > yb) then
         if (e.Graphics.mouse_x > xb1) && (e.Graphics.mouse_x < (xb1+wb)) then
           true
         else
           if (e.Graphics.mouse_x > xb2) && (e.Graphics.mouse_x < (xb2+wb)) then
             false
           else
             attente()
       else
       attente () in
     draw_message s;
     draw_button xb1 yb wb hb "yes";
     draw_button xb2 yb wb hb "no";
     attente()
 
 (* val q_begin : unit -> bool *)
 (* Ask, using function 'question', if the player wishes to start *)
 (* (yes=true) *)
   let q_begin () =
     let b = question "Would you like to begin ?" in
     erase_message();
     b
 
 (* val q_continue : unit -> bool *)
 (* Ask, using function 'question', if the player wishes to play again *)
 (* (yes=true) *)
   let q_continue () =
     let b = question "Play again ?" in
     erase_message();
     b
 
   let q_player () = 
     let b = question "Is there to be a machine player?" in 
     erase_message ();
     b
 (* val won : unit -> unit *)
 (* val lost : unit -> unit *)
 (* val nil : unit -> unit *)
 (* Three functions for these three cases *)
   let won () = 
     draw_message "The first player won" ; ignore (wait_click ()) ; erase_message()
   let lost () = 
     draw_message "The second player won"; ignore (wait_click ()) ; erase_message()
   let nil () = 
     draw_message "Stalemate" ; ignore (wait_click ()) ; erase_message()
 
 (* val init : unit -> unit *)
 (* This is called at every start of the game for the position *) 
   let init  = draw_table
 
   let position b c aj nj  = 
     if b then 
       draw_piece (augment nj c) c cA
     else 
       draw_piece (augment nj c) c cB
         
 (* val drawH : int -> Rep.item array array -> unit *)
 (* Position when the human player chooses move cp in situation j *)
   let drawH cp j =  draw_piece (augment j cp) cp cA
       
 (* val drawM : int -> cell array array -> unit*)
 (* Position when the machine player chooses move cp in situation j *)
   let drawM cp j =  draw_piece (augment j cp) cp cB
 end ;;

(* module C4_graph :
  sig
    type game = C4_rep.game
    and move = C4_rep.move
    val r : int
    val ec : int
    val dec : int
    val cote : int
    val htexte : int
    val width : int
    val height : int
    val height_of_game : int
    val hec : int
    val lec : int
    val margin : int
    val xb1 : int
    val xb2 : int
    val yb : int
    val wb : int
    val hb : int
    val t2e : int -> int
    val cN : Graphics.color
    val cA : Graphics.color
    val cB : Graphics.color
    val cF : Graphics.color
    val draw_table : unit -> unit
    val draw_piece : int -> int -> Graphics.color -> unit
    val augment : C4_rep.cell array array -> int -> int
    val conv : Graphics.status -> int
    val wait_click : unit -> Graphics.status
    val choice : 'a -> C4_rep.cell array array -> int
    val home : unit -> unit
    val exit : unit -> unit
    val draw_button : int -> int -> int -> int -> string -> unit
    val draw_message : string -> unit
    val erase_message : unit -> unit
    val question : string -> bool
    val q_begin : unit -> bool
    val q_continue : unit -> bool
    val q_player : unit -> bool
    val won : unit -> unit
    val lost : unit -> unit
    val nil : unit -> unit
    val init : unit -> unit
    val position : bool -> int -> 'a -> C4_rep.cell array array -> unit
    val drawH : int -> C4_rep.cell array array -> unit
    val drawM : int -> C4_rep.cell array array -> unit
  end *)
