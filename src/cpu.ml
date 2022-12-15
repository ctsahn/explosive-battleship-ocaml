open Core

(* Possible horizontal searches for the CPU, after a hit *)
let horz_attack_queue = Queue.create ()

(* Possible vertical searches for the CPU, after a hit *)
let vert_attack_queue = Queue.create ()

(* The direction the CPU is smartly attacking. "horizontal" or "vertical" after a hit, or "" when no specified direction of attack *)

let attack_direction = ref ""

let rec place_cpu_ships (board : Board.t) (ship_size : int) : unit =
  (* Smallest ship size will be 2*)
  if ship_size >= 1 then
    let board_length = Array.length board in

    let place_row = Random.int board_length in
    let place_col = Random.int board_length in

    (* Randomly select direction to place ship *)
    let dir = Random.int 4 in
    if dir = 0 && place_row + ship_size - 1 < board_length then
      match
        Game.place_ship board "" place_row place_col
          (place_row + ship_size - 1)
          place_col
      with
      | Ok _ ->
          place_cpu_ships board (ship_size - 1)
          (* If placed successfully, place different ship*)
      | Error _ ->
          place_cpu_ships board ship_size (* Try again for the same size ship *)
    else if dir = 1 && place_row - (ship_size - 1) >= 0 then
      match
        Game.place_ship board "" place_row place_col
          (place_row - (ship_size - 1))
          place_col
      with
      | Ok _ -> place_cpu_ships board (ship_size - 1)
      | Error _ -> place_cpu_ships board ship_size
    else if dir = 2 && place_col + (ship_size - 1) < board_length then
      match
        Game.place_ship board "" place_row place_col place_row
          (place_col + (ship_size - 1))
      with
      | Ok _ -> place_cpu_ships board (ship_size - 1)
      | Error _ -> place_cpu_ships board ship_size
    else if dir = 3 && place_col - (ship_size - 1) >= 0 then
      match
        Game.place_ship board "" place_row place_col place_row
          (place_col - (ship_size - 1))
      with
      | Ok _ -> place_cpu_ships board (ship_size - 1)
      | Error _ -> place_cpu_ships board ship_size
    else
      place_cpu_ships board
        ship_size (* No directions available for square, try again *)

(* CPU must find a previously unfired square to attack *)
let rec find_valid_attack (board : Board.t) =
  let board_length = Array.length board in

  let potential_row = Random.int board_length in
  let potential_col = Random.int board_length in

  if Game.is_valid_attack board potential_row potential_col then
    (potential_row, potential_col)
  else find_valid_attack board

let attack_given_coords (user_board : Board.t) (cpu_board : Board.t)
    (attack_row : int) (attack_col : int) : bool =
  if Board.equal_status Board.Ship user_board.(attack_row).(attack_col) then (
    user_board.(attack_row).(attack_col) <- Board.ShipHit;
    (* Clear queues and attack direction after a sink - go back to random search *)
    if Game.has_sunk user_board attack_row attack_col then (
      Queue.clear horz_attack_queue;
      Queue.clear vert_attack_queue;
      attack_direction := "")
    else (
      (* no sink detected - keep queueing potential moves *)

      (* Our current attack direction is horizontal or not yet set, so queue possible horizontal moves (left or right of the hit cell)*)
      if
        String.( = ) !attack_direction ""
        || String.( = ) !attack_direction "horizontal"
      then (
        if Game.is_valid_attack user_board attack_row (attack_col + 1) then
          Queue.enqueue horz_attack_queue (attack_row, attack_col + 1);
        if Game.is_valid_attack user_board attack_row (attack_col - 1) then
          Queue.enqueue horz_attack_queue (attack_row, attack_col - 1));

      (* Our current attack direction is vertical or not yet set, so queue possible vertical moves (above or below of the hit cell)*)
      if
        String.( = ) !attack_direction ""
        || String.( = ) !attack_direction "vertical"
      then (
        if Game.is_valid_attack user_board (attack_row + 1) attack_col then
          Queue.enqueue vert_attack_queue (attack_row + 1, attack_col);
        if Game.is_valid_attack user_board (attack_row - 1) attack_col then
          Queue.enqueue vert_attack_queue (attack_row - 1, attack_col)));

    true)
  else if Board.equal_status Board.Mine user_board.(attack_row).(attack_col)
  then (
    user_board.(attack_row).(attack_col) <- Board.MineHit;
    let penalty_tuple = Game.find_ship cpu_board in
    let penalty_row = fst penalty_tuple in
    let penalty_col = snd penalty_tuple in
    cpu_board.(penalty_row).(penalty_col) <- Board.ShipHit;

    false)
  else (
    user_board.(attack_row).(attack_col) <- Board.Miss;
    false)

(* Core.Queue has no remove function, so use filter to delete a tuple from the queue *)
let remove_from_queue (queue : (int * int) Queue.t) (coord : int * int) : unit =
  Queue.filter_inplace queue ~f:(fun x ->
      match x with row, col -> row <> fst coord || col <> snd coord)

let cpu_use_bomb (user_board : Board.t) (cpu_board : Board.t) (row : int)
    (col : int) =
  let hit_queue = Queue.create () in

  Queue.enqueue hit_queue (attack_given_coords user_board cpu_board row col);
  if Game.is_valid_attack user_board row (col - 1) then
    Queue.enqueue hit_queue
      (attack_given_coords user_board cpu_board row (col - 1));
  if Game.is_valid_attack user_board (row - 1) (col - 1) then
    Queue.enqueue hit_queue
      (attack_given_coords user_board cpu_board (row - 1) (col - 1));
  if Game.is_valid_attack user_board (row + 1) (col - 1) then
    Queue.enqueue hit_queue
      (attack_given_coords user_board cpu_board (row + 1) (col - 1));

  if Game.is_valid_attack user_board (row + 1) col then
    Queue.enqueue hit_queue
      (attack_given_coords user_board cpu_board (row + 1) col);
  if Game.is_valid_attack user_board (row - 1) col then
    Queue.enqueue hit_queue
      (attack_given_coords user_board cpu_board (row - 1) col);

  if Game.is_valid_attack user_board (row + 1) (col + 1) then
    Queue.enqueue hit_queue
      (attack_given_coords user_board cpu_board (row + 1) (col + 1));
  if Game.is_valid_attack user_board row (col + 1) then
    Queue.enqueue hit_queue
      (attack_given_coords user_board cpu_board row (col + 1));
  if Game.is_valid_attack user_board (row - 1) (col + 1) then
    Queue.enqueue hit_queue
      (attack_given_coords user_board cpu_board (row - 1) (col + 1));

  (* filter out everything that is not a valid attack after the bomb attacks *)
  Queue.filter_inplace horz_attack_queue ~f:(fun tup ->
      Game.is_valid_attack user_board (fst tup) (snd tup));
  Queue.filter_inplace vert_attack_queue ~f:(fun tup ->
      Game.is_valid_attack user_board (fst tup) (snd tup));
  Queue.fold hit_queue ~init:false ~f:(fun accum b -> accum || b)

let cpu_attack (user_board : Board.t) (cpu_board : Board.t)
    (bombs_remaining : int ref) : bool =
  if Queue.is_empty vert_attack_queue && Queue.is_empty horz_attack_queue then
    (* No smart moves - just attack randomly *)
    let attack_target = find_valid_attack user_board in
    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    if
      !bombs_remaining > 0
      && (not
            (attack_row = 0 || attack_row = 9 || attack_col = 0
           || attack_col = 9))
      && Random.int 4 = 0
    then (
      bombs_remaining := !bombs_remaining - 1;

      cpu_use_bomb user_board cpu_board attack_row attack_col)
    else attack_given_coords user_board cpu_board attack_row attack_col
  else if Queue.is_empty vert_attack_queue then (
    (* CPU has no vertical moves to make, but does have smart horizontal moves available *)

    (* Pick a random horizontal move from the queue, then delete it so it's no longer available *)
    let attack_target =
      Queue.get horz_attack_queue (Random.int (Queue.length horz_attack_queue))
    in
    remove_from_queue horz_attack_queue attack_target;

    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    (* Only make horizontal moves from now on *)
    attack_direction := "horizontal";

    attack_given_coords user_board cpu_board attack_row attack_col)
  else if Queue.is_empty horz_attack_queue then (
    (* CPU has no horizontal moves to make, but does have smart vertical moves available *)

    (* Pick a random vertical move from the queue, then delete it so it's no longer available *)
    let attack_target =
      Queue.get vert_attack_queue (Random.int (Queue.length vert_attack_queue))
    in
    remove_from_queue vert_attack_queue attack_target;

    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    (* Only make vertical moves from now on *)
    attack_direction := "vertical";

    attack_given_coords user_board cpu_board attack_row attack_col)
  else if String.( = ) !attack_direction "vertical" then (
    (* Attack direction is vertical, so make vertical moves *)
    let attack_target =
      Queue.get vert_attack_queue (Random.int (Queue.length vert_attack_queue))
    in

    remove_from_queue vert_attack_queue attack_target;
    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    attack_given_coords user_board cpu_board attack_row
      attack_col (*horizaontal*))
  else if String.( = ) !attack_direction "horizontal" then (
    (* Attack direction is horizontal, so make horizontal moves *)
    let attack_target =
      Queue.get horz_attack_queue (Random.int (Queue.length horz_attack_queue))
    in

    remove_from_queue horz_attack_queue attack_target;
    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    attack_given_coords user_board cpu_board attack_row attack_col)
  else if Random.int 2 = 1 then (
    (* Both horizontal and vertical queues are not empty, so we have possible squares to attack in both ways*)
    (* Randomly pick horizontal attack direction *)
    attack_direction := "horizontal";
    let attack_target =
      Queue.get horz_attack_queue (Random.int (Queue.length horz_attack_queue))
    in
    remove_from_queue horz_attack_queue attack_target;
    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    attack_given_coords user_board cpu_board attack_row attack_col)
  else (
    (* Randomly pick vertical attack direction *)
    attack_direction := "vertical";
    let attack_target =
      Queue.get vert_attack_queue (Random.int (Queue.length vert_attack_queue))
    in

    remove_from_queue vert_attack_queue attack_target;

    let attack_row = fst attack_target in
    let attack_col = snd attack_target in
    attack_given_coords user_board cpu_board attack_row attack_col)