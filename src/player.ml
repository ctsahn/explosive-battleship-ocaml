open Core
open Board

let player_attack (board : Board.t) (mine_penalty_board : Board.t) (row : int)
    (col : int) (is_single_player : bool) : bool =
  (* hit *)
  if Board.equal_status Board.Ship board.(row).(col) then (
    board.(row).(col) <- Board.ShipHit;
    let _ = Game.has_sunk board row col in
    (* check if the hit ship has sunk - however, we have no use for the returned bool *)
    true)
  else if Board.equal_status Board.Mine board.(row).(col) then (
    board.(row).(col) <- Board.MineHit;
    let penalty_tuple = Game.find_ship mine_penalty_board in
    let penalty_row = fst penalty_tuple in
    let penalty_col = snd penalty_tuple in

    if is_single_player then
      let _ =
        Cpu.attack_given_coords mine_penalty_board board penalty_row penalty_col
      in

      false
    else (
      mine_penalty_board.(penalty_row).(penalty_col) <- ShipHit;
      let _ = Game.has_sunk mine_penalty_board penalty_row penalty_col in
      false))
  else (
    board.(row).(col) <- Board.Miss;
    false)

let player_bomb (board : Board.t) (mine_penalty_board : Board.t) (row : int)
    (col : int) (is_single_player : bool) : bool =
  let hit_queue = Queue.create () in

  Queue.enqueue hit_queue
    (player_attack board mine_penalty_board row col is_single_player);
  if Game.is_valid_attack board row (col - 1) then
    Queue.enqueue hit_queue
      (player_attack board mine_penalty_board row (col - 1) is_single_player);
  if Game.is_valid_attack board (row - 1) (col - 1) then
    Queue.enqueue hit_queue
      (player_attack board mine_penalty_board (row - 1) (col - 1)
         is_single_player);
  if Game.is_valid_attack board (row + 1) (col - 1) then
    Queue.enqueue hit_queue
      (player_attack board mine_penalty_board (row + 1) (col - 1)
         is_single_player);

  if Game.is_valid_attack board (row + 1) col then
    Queue.enqueue hit_queue
      (player_attack board mine_penalty_board (row + 1) col is_single_player);
  if Game.is_valid_attack board (row - 1) col then
    Queue.enqueue hit_queue
      (player_attack board mine_penalty_board (row - 1) col is_single_player);

  if Game.is_valid_attack board (row + 1) (col + 1) then
    Queue.enqueue hit_queue
      (player_attack board mine_penalty_board (row + 1) (col + 1)
         is_single_player);
  if Game.is_valid_attack board row (col + 1) then
    Queue.enqueue hit_queue
      (player_attack board mine_penalty_board row (col + 1) is_single_player);
  if Game.is_valid_attack board (row - 1) (col + 1) then
    Queue.enqueue hit_queue
      (player_attack board mine_penalty_board (row - 1) (col + 1)
         is_single_player);

  Queue.fold hit_queue ~init:false ~f:(fun accum b -> accum || b)
