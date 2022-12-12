open Core
open Board

(* This is where the logic of the game goes *)

(* Possible horizontal searches for the CPU, after a hit *)
let cpu_horz_queue = Queue.create ()

(* Possible vertical searches for the CPU, after a hit *)
let cpu_vert_queue = Queue.create ()

(* The direction the CPU is smartly attacking. "horizontal" or "vertical" after a hit, or "" when no specified direction of attack *)

let cpu_attack_direction = ref ""

(* Error strings, constants *)
let not_straight_error = "not straight"
let touching_error = "touching"
let repeat_error = "repeat"
let too_small_error = "small"
let too_big_error = "big"

(* Create a radius of "Miss" squares when we sink a ship or place a ship, as we know ships cannot touch each other *)
let create_miss_radius (board : Board.t) (row : int) (col : int) : unit =
  (* Fill all 8 squares around a square *)
  if col - 1 >= 0 && equal_status board.(row).(col - 1) Empty then
    board.(row).(col - 1) <- Miss;
  if
    row - 1 >= 0 && col - 1 >= 0 && equal_status board.(row - 1).(col - 1) Empty
  then board.(row - 1).(col - 1) <- Miss;
  if
    row + 1 < Array.length board
    && col - 1 >= 0
    && equal_status board.(row + 1).(col - 1) Empty
  then board.(row + 1).(col - 1) <- Miss;
  if col + 1 < Array.length board && equal_status board.(row).(col + 1) Empty
  then board.(row).(col + 1) <- Miss;
  if
    row - 1 >= 0
    && col + 1 < Array.length board
    && equal_status board.(row - 1).(col + 1) Empty
  then board.(row - 1).(col + 1) <- Miss;
  if
    row + 1 < Array.length board
    && col + 1 < Array.length board
    && equal_status board.(row + 1).(col + 1) Empty
  then board.(row + 1).(col + 1) <- Miss;
  if row + 1 < Array.length board && equal_status board.(row + 1).(col) Empty
  then board.(row + 1).(col) <- Miss;
  if row - 1 >= 0 && equal_status board.(row - 1).(col) Empty then
    board.(row - 1).(col) <- Miss

let rec check_left_right (board : Board.t) (row : int) (col1 : int) (col2 : int)
    : bool =
  (* Check horizontally if there already is a ship placed where the new ship will go. *)
  if col1 > col2 then true
  else if equal_status board.(row).(col1) Ship then false
    (* overlapping other ship *)
  else if equal_status board.(row).(col1) Miss then false
    (* too close to other ships *)
  else check_left_right board row (col1 + 1) col2

let rec check_above_below (board : Board.t) (row1 : int) (row2 : int)
    (col : int) : bool =
  (* Check vertically if there already is a ship placed where the new ship will go. *)
  if row1 > row2 then true
  else if equal_status board.(row1).(col) Ship then false
    (* overlapping other ship *)
  else if equal_status board.(row1).(col) Miss then false
    (* too close to other ships *)
  else check_above_below board (row1 + 1) row2 col

let order_row_col (row1 : int) (col1 : int) (row2 : int) (col2 : int) :
    int * int * int * int =
  (* Get the rows and columns of the start and end clicks *)
  if row1 < row2 then
    if col1 < col2 then (row1, row2, col1, col2) else (row1, row2, col2, col1)
  else if col1 < col2 then (row2, row1, col1, col2)
  else (row2, row1, col2, col1)

let place_ship (board : Board.t) (placed_ships : string) (row1 : int)
    (col1 : int) (row2 : int) (col2 : int) : (int, string) result =
  let row1, row2, col1, col2 = order_row_col row1 col1 row2 col2 in
  if row1 <> row2 && col1 <> col2 then Error not_straight_error
  else if row1 = row2 then
    let ship_size = col2 - col1 + 1 in

    (* Place the horizontal ship *)
    if not (check_left_right board row1 col1 col2) then Error touching_error
    else if ship_size > 5 then Error too_big_error
    else if ship_size < 2 then Error too_small_error
    else if
      String.is_substring placed_ships ~substring:(Int.to_string ship_size)
    then Error repeat_error
    else (
      Array.fill board.(row1) ~pos:col1 ~len:(col2 - col1 + 1) Ship;

      for i = col1 to col2 do
        create_miss_radius board row1 i
      done;

      Ok ship_size)
  else
    let ship_size = row2 - row1 + 1 in
    if (* Place the vertical ship *)
       not (check_above_below board row1 row2 col1)
    then Error touching_error
    else if ship_size > 5 then Error too_big_error
    else if ship_size < 2 then Error too_small_error
    else if
      String.is_substring placed_ships ~substring:(Int.to_string ship_size)
    then Error repeat_error
    else (
      for i = row1 to row2 do
        Array.fill board.(i) ~pos:col1 ~len:1 Ship;
        create_miss_radius board i col1
      done;

      Ok ship_size)

let rec place_cpu_ships (board : Board.t) (ship_size : int): unit =

  (* Smallest ship size will be 2*)
  if ship_size >= 2 then
    let board_length = Array.length board in

    let place_row = Random.int board_length in
    let place_col = Random.int board_length in

    (* Randomly select direction to place ship *)
    let dir = Random.int 4 in
    if dir = 0 && place_row + ship_size - 1 < board_length then
      match
        place_ship board "" place_row
          (place_row + ship_size - 1)
          place_col place_col
      with
      | Ok _ -> place_cpu_ships board (ship_size - 1) (* If placed successfully, place different ship*)
      | Error _ -> place_cpu_ships board ship_size (* Try again for the same size ship *)
    else if dir = 1 && place_row - (ship_size - 1) >= 0 then
      match
        place_ship board "" place_row
          (place_row - (ship_size - 1))
          place_col place_col
      with
      | Ok _ -> place_cpu_ships board (ship_size - 1)
      | Error _ -> place_cpu_ships board ship_size
    else if dir = 2 && place_col + (ship_size - 1) < board_length then
      match
        place_ship board "" place_row
          place_row
          place_col (place_col + (ship_size - 1))
      with
      | Ok _ -> place_cpu_ships board (ship_size - 1)
      | Error _ -> place_cpu_ships board ship_size
    else if dir = 3 && place_col - (ship_size - 1) >=0 then
      match
        place_ship board "" place_row
          place_row
          place_col (place_col - (ship_size - 1))
      with
      | Ok _ -> place_cpu_ships board (ship_size - 1)
      | Error _ -> place_cpu_ships board ship_size
    else 
      place_cpu_ships board ship_size (* No directions available for square, try again *)

let cleanse_board (board : Board.t) : unit =
  (* Only keep ships on the board, everything else Empty *)
  for r = 0 to Array.length board - 1 do
    for c = 0 to Array.length board.(r) - 1 do
      if not (equal_status board.(r).(c) Ship) then board.(r).(c) <- Empty
    done
  done

let is_game_over (board : Board.t) : bool =
  (* Check if unsunken ships still exist *)
  Array.for_all board ~f:(fun row ->
      (not (Array.mem row Board.ShipHit ~equal:Board.equal_status))
      && not (Array.mem row Board.Ship ~equal:Board.equal_status))

let rec check_horizontal_sunk (board : Board.t) (row : int) (col : int)
    (dir : int) : int option =
  (* Check if the horizontal ship has been sunk *)
  if col < 0 || col > Array.length board - 1 then Some (col - dir)
  else if equal_status board.(row).(col) Ship then None
  else if
    equal_status board.(row).(col) Miss || equal_status board.(row).(col) Empty
  then Some (col - dir)
  else check_horizontal_sunk board row (col + dir) dir

let rec check_vertical_sunk (board : Board.t) (row : int) (col : int)
    (dir : int) : int option =
  (* Check if the vertical ship has been sunk *)
  if row < 0 || row > Array.length board - 1 then Some (row - dir)
  else if equal_status board.(row).(col) Ship then None
  else if
    equal_status board.(row).(col) Miss || equal_status board.(row).(col) Empty
  then Some (row - dir)
  else check_vertical_sunk board (row + dir) col dir

let rec sink_horizontal_ship (board : Board.t) (s : int) (e : int) (row : int) :
    bool =
  (* Change the status of the horizontal ship to ShipSunken *)
  if s > e then true
  else (
    board.(row).(s) <- ShipSunken;

    create_miss_radius board row s;

    sink_horizontal_ship board (s + 1) e row)

let rec sink_vertical_ship (board : Board.t) (s : int) (e : int) (col : int) :
    bool =
  (* Change the status of the vertical ship to ShipSunken. *)
  if s > e then true
  else (
    board.(s).(col) <- ShipSunken;

    create_miss_radius board s col;

    sink_vertical_ship board (s + 1) e col)

let has_sunk (board : Board.t) (row : int) (col : int) : bool =
  (* Check if ship is horizontal *)
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
    | _, _ -> false (* Check if ship is vertical *)
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

(* Player attack *)
let attack (board : Board.t) (pos : int) : bool =
  let converted_position = Board.convert_position pos in
  let row = fst converted_position in
  let col = snd converted_position in
  (* hit *)
  if Board.equal_status Board.Ship board.(row).(col) then (
    board.(row).(col) <- Board.ShipHit;
    let _ = has_sunk board row col in
    (* check if the hit ship has sunk - however, we have no use for the returned bool *)
    true)
  else (
    board.(row).(col) <- Board.Miss;
    false)

let is_valid_attack (board : Board.t) (row : int) (col : int) : bool =
  (* Must not be out of bounds, and should not have been fired upon *)
  row >= 0 && col >= 0
  && row < Array.length board
  && col < Array.length board
  && (Board.equal_status Board.Empty board.(row).(col)
     || Board.equal_status Board.Ship board.(row).(col))

(* CPU must find a previously unfired square to attack *)
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
    (* Clear queues and attack direction after a sink - go back to random search *)
    if has_sunk board attack_row attack_col then (
      Queue.clear cpu_horz_queue;
      Queue.clear cpu_vert_queue;
      cpu_attack_direction := "")
    else (
      (* no sink detected - keep queueing potential moves *)

      (* Our current attack direction is horizontal or not yet set, so queue possible horizontal moves (left or right of the hit cell)*)
      if
        String.( = ) !cpu_attack_direction ""
        || String.( = ) !cpu_attack_direction "horizontal"
      then (
        if is_valid_attack board attack_row (attack_col + 1) then
          Queue.enqueue cpu_horz_queue (attack_row, attack_col + 1);
        if is_valid_attack board attack_row (attack_col - 1) then
          Queue.enqueue cpu_horz_queue (attack_row, attack_col - 1));

      (* Our current attack direction is vertical or not yet set, so queue possible vertical moves (above or below of the hit cell)*)
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

(* Core.Queue has no remove function, so use filter to delete a tuple from the queue *)
let remove_from_queue (queue : (int * int) Queue.t) (coord : int * int) : unit =
  Queue.filter_inplace queue ~f:(fun x ->
      match x with row, col -> row <> fst coord || col <> snd coord)

let cpu_attack (board : Board.t) : bool =
  if Queue.is_empty cpu_vert_queue && Queue.is_empty cpu_horz_queue then
    (* No smart moves - just attack randomly *)
    let attack_target = find_valid_attack board in
    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    attack_given_coords board attack_row attack_col
  else if Queue.is_empty cpu_vert_queue then (
    (* CPU has no vertical moves to make, but does have smart horizontal moves available *)

    (* Pick a random horizontal move from the queue, then delete it so it's no longer available *)
    let attack_target =
      Queue.get cpu_horz_queue (Random.int (Queue.length cpu_horz_queue))
    in
    remove_from_queue cpu_horz_queue attack_target;

    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    (* Only make horizontal moves from now on *)
    cpu_attack_direction := "horizontal";

    attack_given_coords board attack_row attack_col)
  else if Queue.is_empty cpu_horz_queue then (
    (* CPU has no horizontal moves to make, but does have smart vertical moves available *)

    (* Pick a random vertical move from the queue, then delete it so it's no longer available *)
    let attack_target =
      Queue.get cpu_vert_queue (Random.int (Queue.length cpu_vert_queue))
    in
    remove_from_queue cpu_vert_queue attack_target;

    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    (* Only make vertical moves from now on *)
    cpu_attack_direction := "vertical";

    attack_given_coords board attack_row attack_col)
  else if String.( = ) !cpu_attack_direction "vertical" then (
    (* Attack direction is vertical, so make vertical moves *)
    let attack_target =
      Queue.get cpu_vert_queue (Random.int (Queue.length cpu_vert_queue))
    in

    remove_from_queue cpu_vert_queue attack_target;
    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    attack_given_coords board attack_row attack_col (*horizaontal*))
  else if String.( = ) !cpu_attack_direction "horizontal" then (
    (* Attack direction is horizontal, so make horizontal moves *)
    let attack_target =
      Queue.get cpu_horz_queue (Random.int (Queue.length cpu_horz_queue))
    in

    remove_from_queue cpu_horz_queue attack_target;
    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    attack_given_coords board attack_row attack_col)
  else if Random.int 2 = 1 then (
    (* Both horizontal and vertical queues are not empty, so we have possible squares to attack in both ways*)
    (* Randomly pick horizontal attack direction *)
    cpu_attack_direction := "horizontal";
    let attack_target =
      Queue.get cpu_horz_queue (Random.int (Queue.length cpu_horz_queue))
    in
    remove_from_queue cpu_horz_queue attack_target;
    let attack_row = fst attack_target in
    let attack_col = snd attack_target in

    attack_given_coords board attack_row attack_col)
  else (
    (* Randomly pick vertical attack direction *)
    cpu_attack_direction := "vertical";
    let attack_target =
      Queue.get cpu_vert_queue (Random.int (Queue.length cpu_vert_queue))
    in

    remove_from_queue cpu_vert_queue attack_target;

    let attack_row = fst attack_target in
    let attack_col = snd attack_target in
    attack_given_coords board attack_row attack_col)
