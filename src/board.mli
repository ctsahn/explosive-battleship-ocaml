type status = Empty | Miss | Ship | ShipHit | ShipSunken

type t = status array array

(*
    Convert the board to a string to be used for requests and responses. 
*)
val board_to_string : t -> string


(* 
    Randomly place ships on a board. 
*)
val initialize_board : t

(* 
    Given the length of the ship and the positions for the starting and ending 
    positions of the ship, place the ship on the player's board. 

    Return true if placement is possible, false otherwise. 
*)
val place_ship : int -> int -> int -> bool

(* 
    Create the boards from a saved game.

    Boolean value is true if playing against the computer, false otherwise.
*)
val load_game : string -> bool -> t * t * bool 

(*
    Save the current game state.

    Boolean value is true if playing against the computer, false otherwise.
*)
val save_game : t -> t -> bool -> string * bool