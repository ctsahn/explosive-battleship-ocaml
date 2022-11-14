(* 
    Initialize the player's board.
*)
val initialize_board : int array array

(*
    Convert the board to a string to be used in the server. 
*)
val board_to_string : int array -> string

(*
    Convert the position in the board (from the server) to x, y coordinates.
*)
val convert_position : int -> int * int

(* 
    Given the length of the ship and the x, y coordinates for the starting and ending 
    positions of the ship, place the ship on the user's board. 

    Return true if placement is possible, false otherwise. 
*)
val place_ship : int -> int -> bool

(* 
    Check whether the position in the board is valid. 
    ie. player does not choose a previously chosen position.
*)
val is_valid_attack : int -> bool

(* 
    Given the position and board to attack, decide whether the player hit a ship or missed. 

    Return true if a ship was hit, false otherwise.
*)
val attack : int -> int array array -> bool

(*
    Given the board to attack and previous hits, the computer chooses a valid position on the 
    opponent's board depending on its previous successful hits. 

    Return true if a ship was hit, false otherwise.
*)
val cpu_attack : int array array -> int * int array -> bool

(*
    Check if a player has sunk all of the opponent's ships. 
*)
val is_win_state : int array array -> bool