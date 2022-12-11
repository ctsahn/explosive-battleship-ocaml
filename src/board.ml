open Core

type status = Empty | Miss | Ship | ShipHit | ShipSunken [@@deriving equal]
type t = status array array

let board_to_string (board : t) : string =
  let l = List.concat @@ List.map ~f:Array.to_list (Array.to_list board) in
  let converted =
    List.map l ~f:(fun ele ->
        match ele with
        | Empty -> "0"
        | Miss -> "1"
        | Ship -> "2"
        | ShipHit -> "3"
        | ShipSunken -> "4")
  in
  String.concat ~sep:"" converted


let populate_board (board:t) (input_str:string):unit = 

  let board_len = Array.length board in 

  Array.iteri board ~f: (fun i _ -> 
    
    let num_list = String.subo ~pos: (i * board_len) ~len:board_len  input_str |> String.to_list in 
    
    List.iteri num_list ~f: (fun j el ->

      match el with 
      | '0' -> board.(i).(j) <- Empty
      | '1' -> board.(i).(j) <- Miss
      | '2' -> board.(i).(j) <- Ship
      | '3' -> board.(i).(j) <- ShipHit
      | '4' -> board.(i).(j) <- ShipSunken
      | _ -> failwith ("None")

      )

    )


let initialize_boards : t * t =
  ( Array.make_matrix ~dimx:10 ~dimy:10 Empty,
    Array.make_matrix ~dimx:10 ~dimy:10 Empty )

let convert_position (pos : int) : int * int = (pos / 10, pos mod 10)

let reset (board:t): unit = 
  Array.iter board ~f:(fun row -> Array.fill row ~pos:0 ~len:10 Empty)

