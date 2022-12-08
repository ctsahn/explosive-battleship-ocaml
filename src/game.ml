open Core
open Board

(* This is where the logic of the game goes *)

let cpu_horz_queue = Queue.create ()
let cpu_vert_queue = Queue.create ()
let cpu_attack_direction = ref ""

let rec check_above_below (board : Board.t) (row : int) (col1 : int)
    (col2 : int) : bool =
  if col1 > col2 then true
  else if col1 > 0 && equal_status board.(row).(col1 - 1) Ship then false
  else if col1 <  Array.length board - 1 && equal_status board.(row).(col1 + 1) Ship then false
  else check_above_below board row (col1 + 1) col2

let rec check_left_right (board : Board.t) (row1 : int) (row2 : int) (col : int)
    : bool =
  if row1 > row2 then true
  else if row1 > 0 && equal_status board.(row1 - 1).(col) Ship then false
  else if row1 <  Array.length board - 1 && equal_status board.(row1 + 1).(col) Ship then false
  else check_left_right board (row1 + 1) row2 col

let place_ship (board : Board.t) (click1 : int) (click2 : int) : bool =
  let row1 = fst (Board.convert_position click1) in
  let col1 = snd (Board.convert_position click1) in
  let row2 = fst (Board.convert_position click2) in
  let col2 = snd (Board.convert_position click2) in

  if row1 <> row2 && col1 <> col2 then false
  else if not (check_above_below board row1 col1 col2) then false
  else if not (check_left_right board row1 row2 col1) then false
  else if row1 = row2 then
    (* horizontal ship *)
    let _ = Array.fill board.(row1) ~pos:col1 ~len:(col2 - col1 + 1) Ship in
    true
  else
    (* vertical ship *)
    let _ =
      for i = row1 to row2 do
        Array.fill board.(i) ~pos:col1 ~len:1 Ship
      done
    in
    true

let is_game_over (board : Board.t) : bool =
  (* Check if unsunken ships still exist *)
  Array.for_all board ~f:(fun row ->
      (not (Array.mem row Board.ShipHit ~equal:Board.equal_status))
      && not (Array.mem row Board.Ship ~equal:Board.equal_status))

let rec check_horizontal_sunk (board : Board.t) (row : int) (col : int)
    (dir : int) : int option =
  if col < 0 || col > Array.length board - 1 then Some (col - dir)
  else if equal_status board.(row).(col) Ship then None
  else if
    equal_status board.(row).(col) Miss || equal_status board.(row).(col) Empty
  then Some (col - dir)
  else check_horizontal_sunk board row (col + dir) dir

let rec check_vertical_sunk (board : Board.t) (row : int) (col : int)
    (dir : int) : int option =
  if row < 0 || row > Array.length board - 1 then Some (row - dir)
  else if equal_status board.(row).(col) Ship then None
  else if
    equal_status board.(row).(col) Miss || equal_status board.(row).(col) Empty
  then Some (row - dir)
  else check_vertical_sunk board (row + dir) col dir

let rec sink_horizontal_ship (board : Board.t) (s : int) (e : int) (row : int) :
    bool =
  if s > e then true
  else
    let _ = board.(row).(s) <- ShipSunken in
    sink_horizontal_ship board (s + 1) e row

let rec sink_vertical_ship (board : Board.t) (s : int) (e : int) (col : int) :
    bool =
  if s > e then true
  else
    let _ = board.(s).(col) <- ShipSunken in
    sink_vertical_ship board (s + 1) e col

let has_sunk (board : Board.t) (row : int) (col : int) : bool =
  (* horizontal *)
  if
    col > 0
    && (equal_status board.(row).(col - 1) Ship
       || equal_status board.(row).(col - 1) ShipHit)
  then
    match
      ( check_horizontal_sunk board row (col - 1) (-1),
        check_horizontal_sunk board row (col + 1) 1 )
    with
    | Some s, Some e -> sink_horizontal_ship board s e row
    | _, _ -> false
  else if
    col < Array.length board - 1
    && (equal_status board.(row).(col + 1) Ship
       || equal_status board.(row).(col + 1) ShipHit)
  then
    match
      ( check_horizontal_sunk board row (col - 1) (-1),
        check_horizontal_sunk board row (col + 1) 1 )
    with
    | Some s, Some e -> sink_horizontal_ship board s e row
    | _, _ -> false (* vertical *)
  else if
    row > 0
    && (equal_status board.(row - 1).(col) Ship
       || equal_status board.(row - 1).(col) ShipHit)
  then
    match
      ( check_vertical_sunk board (row - 1) col (-1),
        check_vertical_sunk board (row + 1) col 1 )
    with
    | Some s, Some e -> sink_vertical_ship board s e col
    | _, _ -> false
  else if
    row < Array.length board - 1
    && (equal_status board.(row + 1).(col) Ship
       || equal_status board.(row + 1).(col) ShipHit)
  then
    match
      ( check_vertical_sunk board (row - 1) col (-1),
        check_vertical_sunk board (row + 1) col 1 )
    with
    | Some s, Some e -> sink_vertical_ship board s e col
    | _, _ -> false
  else false

let attack (board : Board.t) (pos : int) : bool =
  let converted_position = Board.convert_position pos in
  let row = fst converted_position in
  let col = snd converted_position in
  (* hit *)
  if Board.equal_status Board.Ship board.(row).(col) then (
    board.(row).(col) <- Board.ShipHit;
    let _ = has_sunk board row col in
    (* check if the hit ship has sunk *)
    true
    (* miss *))
  else (
    board.(row).(col) <- Board.Miss;
    false)

let is_valid_attack (board : Board.t) (row : int) (col : int) : bool =
  row >= 0 && col >= 0
  && row < Array.length board
  && col < Array.length board
  && (Board.equal_status Board.Empty board.(row).(col)
     || Board.equal_status Board.Ship board.(row).(col))

let rec find_valid_attack (board : Board.t) =
  let board_length = Array.length board in

  let potential_row = Random.int board_length in
  let potential_col = Random.int board_length in

  if is_valid_attack board potential_row potential_col then
    (potential_row, potential_col)
  else find_valid_attack board

let attack_given_coords (board : Board.t) (attack_row : int) (attack_col : int)
    : bool =
  if Board.equal_status Board.Ship board.(attack_row).(attack_col) then (
    board.(attack_row).(attack_col) <- Board.ShipHit;

    (* Reset queues and attack direction after a sink *)
    if has_sunk board attack_row attack_col then (
      Queue.clear cpu_horz_queue;
      Queue.clear cpu_vert_queue;
      cpu_attack_direction := "" (* no sink detected - keep queueing *))
    else (
      if
        String.( = ) !cpu_attack_direction ""
        || String.( = ) !cpu_attack_direction "horizontal"
      then (
        if is_valid_attack board attack_row (attack_col + 1) then
          Queue.enqueue cpu_horz_queue (attack_row, attack_col + 1);
        if is_valid_attack board attack_row (attack_col - 1) then
          Queue.enqueue cpu_horz_queue (attack_row, attack_col - 1));

      if
        String.( = ) !cpu_attack_direction ""
        || String.( = ) !cpu_attack_direction "vertical"
      then (
        if is_valid_attack board (attack_row + 1) attack_col then
          Queue.enqueue cpu_vert_queue (attack_row + 1, attack_col);
        if is_valid_attack board (attack_row - 1) attack_col then
          Queue.enqueue cpu_vert_queue (attack_row - 1, attack_col)));

    true)
  else (
    board.(attack_row).(attack_col) <- Board.Miss;

    false)

let remove_from_queue (queue : (int * int) Queue.t) (coord : int * int) : unit =
  Queue.filter_inplace queue ~f:(fun x ->
      match x with row, col -> row <> fst coord || col <> snd coord)

let cpu_attack (board : Board.t) : bool =
  if Queue.is_empty cpu_vert_queue && Queue.is_empty cpu_horz_queue then
    let attack_target = find_valid_attack board in
    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    attack_given_coords board attack_row attack_col
  else if Queue.is_empty cpu_vert_queue then (
    (*let attack_target = Queue.dequeue_exn cpu_horz_queue in *)
    let attack_target =
      Queue.get cpu_horz_queue (Random.int (Queue.length cpu_horz_queue))
    in

    remove_from_queue cpu_horz_queue attack_target;

    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    cpu_attack_direction := "horizontal";

    attack_given_coords board attack_row attack_col)
  else if Queue.is_empty cpu_horz_queue then (
    (*let attack_target = Queue.dequeue_exn cpu_vert_queue in*)
    let attack_target =
      Queue.get cpu_vert_queue (Random.int (Queue.length cpu_vert_queue))
    in

    remove_from_queue cpu_vert_queue attack_target;

    let attack_row = fst attack_target in
    let attack_col = snd attack_target in
    cpu_attack_direction := "vertical";

    attack_given_coords board attack_row attack_col
    (*both horz and vert queue have are not empty and we have possible coords to attack in both ways*))
  else if String.( = ) !cpu_attack_direction "" then
    (* pick horz or vert randomly *)
    if Random.int 2 = 1 then (
      cpu_attack_direction := "horizontal";
      (*let attack_target = Queue.dequeue_exn cpu_horz_queue in*)
      let attack_target =
        Queue.get cpu_horz_queue (Random.int (Queue.length cpu_horz_queue))
      in
      remove_from_queue cpu_horz_queue attack_target;

      let attack_row = fst attack_target in
      let attack_col = snd attack_target in

      attack_given_coords board attack_row attack_col)
    else (
      cpu_attack_direction := "vertical";
      (*let attack_target = Queue.dequeue_exn cpu_vert_queue in*)
      let attack_target =
        Queue.get cpu_vert_queue (Random.int (Queue.length cpu_vert_queue))
      in

      remove_from_queue cpu_vert_queue attack_target;

      let attack_row = fst attack_target in
      let attack_col = snd attack_target in

      attack_given_coords board attack_row attack_col)
  else if String.( = ) !cpu_attack_direction "vertical" then (
    (*let attack_target = Queue.dequeue_exn cpu_vert_queue in*)
    let attack_target =
      Queue.get cpu_vert_queue (Random.int (Queue.length cpu_vert_queue))
    in

    remove_from_queue cpu_vert_queue attack_target;
    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    attack_given_coords board attack_row attack_col (*horizaontal*))
  else
    (*let attack_target = Queue.dequeue_exn cpu_horz_queue in*)
    let attack_target =
      Queue.get cpu_horz_queue (Random.int (Queue.length cpu_horz_queue))
    in

    remove_from_queue cpu_horz_queue attack_target;
    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    attack_given_coords board attack_row attack_col