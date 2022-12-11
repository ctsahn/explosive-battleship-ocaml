type status = Empty | Miss | Ship | ShipHit | ShipSunken [@@deriving equal]

(* Treat board as a 2D array *)
type t = status array array

(*
    Convert the board to a string to be used for requests and responses. 
*)
val board_to_string : t -> string

(*
     Create a 10x10 array of "Empty" squares
*)
val populate_board : t -> string -> unit

(*
     Create two 10x10 arrays of "Empty" squares for each player
*)
val initialize_boards : t * t

(*
    Convert the single integer position from the server to x, y coordinates (row, col) for a 2D array.
*)
val convert_position : int -> int * int

val reset: t -> unit