open Core

type status = Empty | Miss | Ship | ShipHit | ShipSunken [@@deriving equal]

type t = status array array

let board_to_string (board: t) : string = 
  
  let l = List.concat @@ (List.map ~f:Array.to_list (Array.to_list board)) in 
  let converted = 
    List.map l
      ~f:(fun ele ->
        match ele with 
        | Empty -> "0"
        | Miss -> "1"
        | Ship -> "2"
        | ShipHit -> "3" 
        | ShipSunken -> "4")
  in 
  String.concat ~sep:"" converted

let initialize_boards : t*t =
  ((Array.make_matrix ~dimx:10 ~dimy:10 Empty), (Array.make_matrix ~dimx:10 ~dimy:10 Empty))
  
let convert_position (pos: int) : int * int = (pos / 10, pos mod 10)