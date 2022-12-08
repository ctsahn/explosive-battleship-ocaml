open Core
open Board

(* This is where the logic of the game goes *)

(* place ship, sink ship - add radius (change to miss) *)

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

let place_ship (board:Board.t) (click1:int) (click2:int): int = 
  let row1 = fst (Board.convert_position click1) in
  let col1 = snd (Board.convert_position click1) in
  let row2 = fst (Board.convert_position click2) in 
  let col2 = snd (Board.convert_position click2) in

  if ((row1 <> row2) && (col1 <> col2)) then -1 
  else if not (check_above_below board row1 col1 col2) then -1 
  else if not (check_left_right board row1 row2 col1) then -1
  else if (row1 = row2) then (* horizontal ship *)
    let _ = Array.fill (board.(row1)) ~pos:col1 ~len: (col2 - col1 + 1) Ship in 
    col2 - col1 + 1
  else (* vertical ship *)
    let _ = 
      for i = row1 to row2 do 
        Array.fill (board.(i)) ~pos:col1 ~len:1 Ship
      done 
    in
    row2 - row1 + 1

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


    
 let rec check_horizontal_sunk (board: Board.t) (row: int) (col: int) (dir: int) : int option = 
  if col < 0 || col > 9 then Some (col - dir)
  else if equal_status board.(row).(col) Ship then None 
  else if (equal_status board.(row).(col) Miss) || (equal_status board.(row).(col) Empty) then Some (col - dir)
  else check_horizontal_sunk board row (col + dir) dir

let rec check_vertical_sunk (board: Board.t) (row: int) (col: int) (dir: int) : int option = 
  if row < 0 || row > (Array.length board - 1) then Some (row - dir)
  else if equal_status board.(row).(col) Ship then None 
  else if (equal_status board.(row).(col) Miss) || (equal_status board.(row).(col) Empty) then Some (row - dir)
  else check_vertical_sunk board (row + dir) col dir

let rec sink_horizontal_ship (board: Board.t) (s: int) (e: int) (row: int) : bool = 
  if s > e then true 
  else let _ = board.(row).(s) <- ShipSunken in sink_horizontal_ship board (s + 1) e row

let rec sink_vertical_ship (board: Board.t) (s: int) (e: int) (col: int) : bool = 
  if s > e then true 
  else let _ = board.(s).(col) <- ShipSunken in sink_vertical_ship board (s + 1) e col

let has_sunk (board: Board.t) (row: int) (col: int) : bool = 
  (* horizontal *)
  if col > 0 && ((equal_status board.(row).(col - 1) Ship) || (equal_status board.(row).(col - 1) ShipHit)) then 
    match check_horizontal_sunk board row (col - 1) (-1), check_horizontal_sunk board row (col + 1) 1 with 
    | Some s, Some e -> sink_horizontal_ship board s e row
    | _, _ -> false
  else if col < (Array.length board - 1) && ((equal_status board.(row).(col + 1) Ship) || (equal_status board.(row).(col + 1) ShipHit)) then
    match check_horizontal_sunk board row (col - 1) (-1), check_horizontal_sunk board row (col + 1) 1 with 
    | Some s, Some e -> sink_horizontal_ship board s e row
    | _, _ -> false

  (* vertical *)
  else if row > 0 && ((equal_status board.(row - 1).(col) Ship) || (equal_status board.(row - 1).(col) ShipHit)) then
    match check_vertical_sunk board (row - 1) col (-1), check_vertical_sunk board (row + 1) col 1 with 
    | Some s, Some e -> sink_vertical_ship board s e col
    | _, _ -> false
  else if row < (Array.length board - 1) && ((equal_status board.(row + 1).(col) Ship) || (equal_status board.(row + 1).(col) ShipHit)) then
    match check_vertical_sunk board (row - 1) col (-1), check_vertical_sunk board (row + 1) col 1 with 
    | Some s, Some e -> sink_vertical_ship board s e col
    | _, _ -> false

  else false




   

