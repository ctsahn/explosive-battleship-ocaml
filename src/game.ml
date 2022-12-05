open Core
open Board

(* This is where the logic of the game goes *)

let rec check_above_below (board:Board.t) (row:int) (col1:int) (col2:int) : bool = 
  if col1 > col2 then true
  else if col1 > 0 && equal_status board.(row).(col1 - 1) Ship then false
  else if col1 < 9 && equal_status board.(row).(col1 + 1) Ship then false
  else check_above_below board row (col1 + 1) col2

let rec check_left_right (board:Board.t) (row1:int) (row2:int) (col:int) : bool = 
  if row1 > row2 then true 
  else if row1 > 0 && equal_status board.(row1 - 1).(col) Ship then false
  else if row1 < 9 && equal_status board.(row1 + 1).(col) Ship then false
  else check_left_right board (row1 + 1) row2 col

let place_ship (board:Board.t) (click1:int) (click2:int): bool = 
  let row1 = fst (Board.convert_position click1) in
  let col1 = snd (Board.convert_position click1) in
  let row2 = fst (Board.convert_position click2) in 
  let col2 = snd (Board.convert_position click2) in

  if ((row1 <> row2) && (col1 <> col2)) then false 
  else if not (check_above_below board row1 col1 col2) then false 
  else if not (check_left_right board row1 row2 col1) then false
  else if (row1 = row2) then (* horizontal ship *)
    let _ = Array.fill (board.(row1)) ~pos:col1 ~len: (col2 - col1 + 1) Ship in 
    true
  else (* vertical ship *)
    let _ = 
      for i = row1 to row2 do 
        Array.fill (board.(i)) ~pos:col1 ~len:1 Ship
      done 
    in
    true

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


    
 let rec check_horizontal_sunk (board:Board.t) (row: int) (col: int) (dir: int) : bool = 
  if col < 0 || col > 9 then true
  else if equal_status board.(row).(col) Ship then false 
  else if (equal_status board.(row).(col) Miss) || (equal_status board.(row).(col) Empty) then true
  else check_horizontal_sunk board row (col + dir) dir

let rec check_vertical_sunk (board:Board.t) (row: int) (col: int) (dir: int) : bool = 
  if row < 0 || row > 9 then true
  else if equal_status board.(row).(col) Ship then false 
  else if (equal_status board.(row).(col) Miss) || (equal_status board.(row).(col) Empty) then true
  else check_vertical_sunk board (row + dir) col dir

let has_sunk (board:Board.t) (row: int) (col: int) : bool = 
  (* horizontal *)
  if col > 0 && ((equal_status board.(row).(col - 1) Ship) || (equal_status board.(row).(col - 1) ShipHit)) then 
    check_horizontal_sunk board row (col - 1) (-1) && check_horizontal_sunk board row (col + 1) 1
  else if col < 9 && ((equal_status board.(row).(col + 1) Ship) || (equal_status board.(row).(col + 1) ShipHit)) then
    check_horizontal_sunk board row (col - 1) (-1) && check_horizontal_sunk board row (col + 1) 1

  (* vertical *)
  else if row > 0 && ((equal_status board.(row - 1).(col) Ship) || (equal_status board.(row + 1).(col) ShipHit)) then
    check_vertical_sunk board (row - 1) col (-1) && check_vertical_sunk board (row + 1) col 1
  else if row < 9 && ((equal_status board.(row + 1).(col) Ship) || (equal_status board.(row + 1).(col) ShipHit)) then
    check_vertical_sunk board (row - 1) col (-1) && check_vertical_sunk board (row + 1) col 1

  else false




   

