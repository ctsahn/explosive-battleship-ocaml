(** Status of a square/cell *)
type status = Empty | Miss | Ship | ShipHit | ShipSunken | Mine | MineHit
[@@deriving equal]

type t = status array array
(** Treat board as a 2D array *)

(**
    Convert the board to a string to be used for requests and responses. 
*)
val board_to_string : t -> string

(**
    Create a 10x10 array, setting the status of each square depending on the input string. 
    This is for creating the boards when the player loads a saved game. 
*)
val populate_board : t -> string -> unit

(**
    Create two 10x10 arrays of "Empty" squares for each player.
*)
val initialize_boards : t * t

(**
    Convert the single integer position from the server to coordinates (row, col) for a 2D array.
*)
val convert_position : int -> int * int

(**
     Clear the board, setting all squares to Empty.
*)
val reset : t -> unit
