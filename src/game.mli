(* 
    Given the positions for the starting and ending 
    positions of the ship, place the ship on the player's board. 

    Return length of ship if placement is possible, -1 otherwise. 
*)
val place_ship : Board.t -> int -> int -> int

(* 
    Given the position and board to attack, decide whether the player hit a ship or missed. 

    Return true if a ship was hit, false otherwise.
*)
val attack : Board.t -> int -> bool


(*
    Check if a player has sunk all of the opponent's ships. 
*)
val is_game_over : Board.t -> bool


(*
    Given the board to attack and previous hits, the computer chooses a valid position on the 
    opponent's board depending on its previous successful hits. 

    Return true if a ship was hit, false otherwise.
*)
val cpu_attack : Board.t  -> bool

val has_sunk : Board.t -> int -> int -> bool 

(*
(*
    Randomly place CPU ships on a board.
*)
val place_cpu_ships: Board.t

(* 
    Check whether the position in the board is valid. 
    ie. player does not choose a previously chosen position.
*)
val is_valid_attack : int -> bool






(* 
    Create the boards from a saved game.

    Boolean value is true if playing against the computer, false otherwise.
*)
val load_game : string -> bool -> Board.t * Board.t * bool 

(*
    Save the current game state.

    Boolean value is true if playing against the computer, false otherwise.
*)
val save_game : Board.t -> Board.t -> bool -> string * bool *)
