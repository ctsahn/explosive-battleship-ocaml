(*
    Convert the position in the board (from the server) to x, y coordinates for a 2D array.
*)
val convert_position : int -> int * int

(* 
    Check whether the position in the board is valid. 
    ie. player does not choose a previously chosen position.
*)
val is_valid_attack : int -> bool

(* 
    Given the position and board to attack, decide whether the player hit a ship or missed. 

    Return true if a ship was hit, false otherwise.
*)
val attack : int -> Board.t -> bool

(*
    Given the board to attack and previous hits, the computer chooses a valid position on the 
    opponent's board depending on its previous successful hits. 

    Return true if a ship was hit, false otherwise.
*)
val cpu_attack : Board.t -> int * int array -> bool

(*
    Check if a player has sunk all of the opponent's ships. 
*)
val is_win_state : Board.t -> bool
