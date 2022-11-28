type status = Empty | Miss | Ship | ShipHit | ShipSunken

type t = status array array

let board_to_string (board: t) : string = 
  let l = List.concat @@ (List.map Array.to_list (Array.to_list board)) in 
  let converted = 
    List.map 
      (fun ele ->
        match ele with 
        | Empty -> "0"
        | Miss -> "1"
        | Ship -> "2"
        | ShipHit -> "3" 
        | ShipSunken -> "4")
      l
  in 
  String.concat "" converted

let initialize_board : t =
  Array.make_matrix 10 10 Empty

let convert_position (pos: int) : int * int = (pos mod 10, pos / 10)