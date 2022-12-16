val not_straight_error : string
val touching_error : string
val repeat_error : string

val too_big_error : string

(*
     Given the length of the ship and the positions for the starting and ending
     positions of the ship, place the ship on the player's board.

     Return true if placement is possible, false otherwise.
*)
val place_ship :
  Board.t -> string -> int -> int -> int -> int -> (int, string) result

(*
     Given the position and board to attack, decide whether the player hit a ship or missed.

     Return true if a ship was hit, false otherwise.
*)

(*
    Check if a player has sunk all of the opponent's ships. 
*)
val is_game_over : Board.t -> bool

(*
    Check if a hit resulted in the sinking of the ship. This also updates the ship status to ShipSunken when appropriate.
*)
val has_sunk : Board.t -> int -> int -> bool

(*
     Check whether a position in the board is able to be attacked (not previously attacked, not out of bounds, etc).
*)
val is_valid_attack : Board.t -> int -> int -> bool
val cleanse_board : Board.t -> unit

val save_single_player_game :
  Board.t ->
  Board.t ->
  int ->
  int ->
  string ->
  (int * int) Core.Queue.t ->
  (int * int) Core.Queue.t ->
  string ->
  unit

val save_two_player_game : Board.t -> Board.t -> int -> int -> string -> unit

(* True if two player game *)
val load_game :
  Board.t ->
  Board.t ->
  int ref ->
  int ref ->
  string ref ->
  (int * int) Core.Queue.t ->
  (int * int) Core.Queue.t ->
  string ref ->
  bool

val find_ship : Board.t -> int * int

(*
     Create the boards from a saved game.

     Boolean value is true if playing against the computer, false otherwise.
*)

(*
    Save the current game state.

    Boolean value is true if playing against the computer, false otherwise.
*)
