open Core
open Board

(* This is where the logic of the game goes *)

(*let cpu_attack_list = []*)
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

let is_game_over (board:Board.t) : bool = 
  (* Check if unsunken ships still exist *)
  Array.for_all board ~f:(fun row -> not(Array.mem (row) (Board.ShipHit) ~equal: Board.equal_status) && not(Array.mem (row) (Board.Ship) ~equal: Board.equal_status))




let cpu_attack (board:Board.t) : bool = 

  

    let board_length = Array.length board in 

    let attack_x = Random.int board_length in 
    let attack_y = Random.int board_length in 

    if Board.equal_status Board.Ship board.(attack_x).(attack_y) then (
      board.(attack_x).(attack_y) <- Board.ShipHit; (*TODO: sink logic*)
      true)
      
    (* miss *)
    else(
       board.(attack_x).(attack_y) <- Board.Miss; 
       false)


    
  




   

