type status = Empty | Miss | Ship | ShipHit | ShipSunken  [@@deriving equal]

(* Treat board as a 2D array *)
type t = status array array

(*
    Convert the board to a string to be used for requests and responses. 
*)
val board_to_string : t -> string


(* 
    Create a 10x10 array of "Empty" squares
*)
val initialize_boards : t*t


(*
    Convert the position in the board (from the server) to x, y coordinates for a 2D array.
*)
val convert_position : int -> int * int