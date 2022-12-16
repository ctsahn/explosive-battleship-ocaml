val not_straight_error : string
val touching_error : string
val repeat_error : string
val too_big_error : string

(*
     Given the length of the ship and the positions for the starting and ending
     positions of the ship, place the ship on the player's board.

     Return Ok <length of ship> if placement is possible, Error <message> otherwise
*)
val place_ship : Board.t -> string -> int -> int -> int -> int -> (int, string) result

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

(* 
     Clear the board of anything but ships. Everything else updated to Empty. 
*)
val cleanse_board : Board.t -> unit

(*
     Save the current single-player game state.
*)
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

(*
     Save the current 2-player game state.
*)
val save_two_player_game : Board.t -> Board.t -> int -> int -> string -> unit

(*
     Create the boards from a saved game.

     Boolean value is true if it's a 2-player game, false otherwise.
*)
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

(* 
     Find a position on the board that has status Ship. 
*)
val find_ship : Board.t -> int * int
