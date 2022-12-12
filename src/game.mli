val cpu_horz_queue : (int * int) Core.Queue.t
val cpu_vert_queue : (int * int) Core.Queue.t
val cpu_attack_direction : string ref


val not_straight_error : string
val touching_error : string
val repeat_error : string
val too_small_error : string
val too_big_error : string

(*
     Given the length of the ship and the positions for the starting and ending
     positions of the ship, place the ship on the player's board.

     Return true if placement is possible, false otherwise.
*)
val place_ship : Board.t -> string -> int -> int -> (int, string) result

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
    Check if a hit resulted in the sinking of the ship. This also updates the ship status to ShipSunken when appropriate.
*)
val has_sunk : Board.t -> int -> int -> bool

(*
    Given the board to attack and previous hits, the computer chooses a valid position on the 
    opponent's board depending on its previous successful hits. 

    Return true if a ship was hit, false otherwise.
*)
val cpu_attack : Board.t -> bool

(* 
    Check whether a position in the board is able to be attacked (not previously attacked, not out of bounds, etc).
*)
val is_valid_attack: Board.t -> int -> int -> bool

val cleanse_board: Board.t -> unit


(*
(*
    Randomly place CPU ships on a board.
*)
val place_cpu_ships: Board.t



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
