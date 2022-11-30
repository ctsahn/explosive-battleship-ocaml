open Core
open Board

(* This is where the logic of the game goes *)

let place_ship (board:Board.t) (click1:int) (click2:int): bool = 

  (*Only works for left to right clicks for now*)
  let row1 = fst (Board.convert_position click1) in
  let col1 = snd (Board.convert_position click1) in

  let col2 = snd (Board.convert_position click2) in
  Array.fill (board.(row1)) ~pos:col1 ~len: (col2 - col1 +1) Ship;

  true (* false if it is not valid *)

let attack (board:Board.t) (pos:int) : bool = 
  let converted_position = Board.convert_position pos in 
  let x = fst converted_position in 
  let y = snd converted_position in 
  (* hit *)
  if Board.equal_status Board.Ship board.(x).(y) then (
    board.(x).(y) <- Board.ShipHit; (*TODO: sink logic*)
    true)
  (* miss *)
  else(
     board.(x).(y) <- Board.Miss; 
     false) 
