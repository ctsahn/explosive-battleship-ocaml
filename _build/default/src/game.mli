(* 
    Initialize the player's board.
*)
val initialize_board : int array array

(*
    Convert the board to a string to be used in the server. 
*)
val board_to_string : int array -> string

(* 
    Given the length of the ship and the x, y coordinates for the starting and ending 
    positions of the ship, place the ship on the user's board. 

    Return true if placement is possible, false otherwise. 
*)
val place_ship : int -> int -> bool

(*
    Convert the position in the board (from the server) to x, y coordinates.
*)
val convert_position : int -> int * int

(* 
    Check whether the position in the board is valid, ie. choosing a previously chosen position.
*)
val is_valid_attack : int -> bool

(* 
    Given the position and board, decide whether the player hit a ship or missed. 
*)
val attack : int -> int array array -> bool

(*
    Given the board and previous hits, the computer attacks a valid position on the opponent's 
    board depending on its previous successful hits. 

    Return true if a ship was hit, false otherwise.
*)
val cpu_attack : int array array -> int * int array -> bool

(*
    Check if a player has sunk all of the opponent's ships. 
*)
val is_win_state : int array array -> bool