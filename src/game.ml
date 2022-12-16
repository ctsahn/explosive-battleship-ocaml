open Core
open Board

(* This is where the logic of the game goes *)

(* Error strings, constants *)
let not_straight_error = "not straight"
let touching_error = "touching"
let repeat_error = "repeat"
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
    else if
      String.is_substring placed_ships ~substring:(Int.to_string ship_size)
    then Error repeat_error
    else if ship_size = 1 then (
      board.(row1).(col1) <- Mine;
      create_miss_radius board row1 col1;
      Ok 1)
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
    else if
      String.is_substring placed_ships ~substring:(Int.to_string ship_size)
    then Error repeat_error
    else (
      for i = row1 to row2 do
        Array.fill board.(i) ~pos:col1 ~len:1 Ship;
        create_miss_radius board i col1
      done;

      Ok ship_size)

let cleanse_board (board : Board.t) : unit =
  (* Only keep ships on the board, everything else Empty *)
  for r = 0 to Array.length board - 1 do
    for c = 0 to Array.length board.(r) - 1 do
      if
        (not (equal_status board.(r).(c) Ship))
        && not (equal_status board.(r).(c) Mine)
      then board.(r).(c) <- Empty
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

let is_valid_attack (board : Board.t) (row : int) (col : int) : bool =
  (* Must not be out of bounds, and should not have been fired upon *)
  row >= 0 && col >= 0
  && row < Array.length board
  && col < Array.length board
  && (Board.equal_status Board.Empty board.(row).(col)
     || Board.equal_status Board.Ship board.(row).(col)
     || Board.equal_status Board.Mine board.(row).(col))

let rec find_ship (board : Board.t) : int * int =
  let board_length = Array.length board in

  let potential_row = Random.int board_length in
  let potential_col = Random.int board_length in

  if Board.equal_status Board.Ship board.(potential_row).(potential_col) then
    (potential_row, potential_col)
  else find_ship board

(* Player attack *)

type two_player_save = {
  player1 : string;
  player2 : string;
  player1_bombs : int;
  player2_bombs : int;
  turn : string;
}
[@@deriving yojson { exn = true }]

type single_player_save = {
  user : string;
  cpu : string;
  user_bombs : int;
  cpu_bombs : int;
  turn : string;
  cpu_horz_queue : string;
  cpu_vert_queue : string;
  cpu_direction : string;
}
[@@deriving yojson { exn = true }]

let save_single_player_game (user_board : Board.t) (cpu_board : Board.t)
    (user_bombs : int) (cpu_bombs : int) (current_turn : string)
    (cpu_horz_queue : (int * int) Core.Queue.t)
    (cpu_vert_queue : (int * int) Core.Queue.t) (cpu_attack_direction : string)
    =
  let tuple_to_string (tup : int * int) =
    Int.to_string (fst tup) ^ "," ^ Int.to_string (snd tup)
  in

  let save_states =
    {
      user = Board.board_to_string user_board;
      cpu = Board.board_to_string cpu_board;
      user_bombs;
      cpu_bombs;
      turn = current_turn;
      cpu_horz_queue =
        List.to_string ~f:tuple_to_string (Queue.to_list cpu_horz_queue);
      cpu_vert_queue =
        List.to_string ~f:tuple_to_string (Queue.to_list cpu_vert_queue);
      cpu_direction = cpu_attack_direction;
    }
  in

  let out_str =
    single_player_save_to_yojson save_states |> Yojson.Safe.to_string
  in
  Out_channel.write_all "saved.txt" ~data:out_str

let save_two_player_game (player1_board : Board.t) (player2_board : Board.t)
    (player1_bombs : int) (player2_bombs : int) (current_turn : string) =
  let save_states =
    {
      player1 = Board.board_to_string player1_board;
      player2 = Board.board_to_string player2_board;
      player1_bombs;
      player2_bombs;
      turn = current_turn;
    }
  in
  let out_str =
    two_player_save_to_yojson save_states |> Yojson.Safe.to_string
  in
  Out_channel.write_all "saved.txt" ~data:out_str

let load_game (player1_board : Board.t) (player2_board : Board.t)
    (player1_bombs : int ref) (player2_bombs : int ref)
    (current_turn : string ref) (cpu_horz_queue : (int * int) Core.Queue.t)
    (cpu_vert_queue : (int * int) Core.Queue.t)
    (cpu_attack_direction : string ref) : bool =
  let load_json = In_channel.read_all "saved.txt" |> Yojson.Safe.from_string in

  (* Load it as a 2-player record first *)
  let record = two_player_save_of_yojson load_json in
  match record with
  | Ok two_player_record ->
      current_turn := two_player_record.turn;
      player1_bombs := two_player_record.player1_bombs;
      player2_bombs := two_player_record.player2_bombs;
      Board.populate_board player1_board two_player_record.player1;
      Board.populate_board player2_board two_player_record.player2;

      true
  (* Error thrown, so must be single player record *)
  | Error _ ->
      (* Convert a the saved queue string to a list, so we can populate the CPU queues *)
      let queue_string_to_list (str : string) : (int * int) list =
        (* Empty queues *)
        if String.( = ) str "()" then []
        else
          let filtered_list =
            String.chop_prefix_exn ~prefix:"(" str
            |> String.chop_suffix_exn ~suffix:")"
            |> String.split ~on:' '
          in

          List.map filtered_list ~f:(fun tuple_str ->
              let row_col_list = String.split tuple_str ~on:',' in

              let row_str = List.hd_exn row_col_list in
              let col_str = List.last_exn row_col_list in

              (Int.of_string row_str, Int.of_string col_str))
      in
      let single_player_record = single_player_save_of_yojson_exn load_json in
      current_turn := single_player_record.turn;
      player1_bombs := single_player_record.user_bombs;
      player2_bombs := single_player_record.cpu_bombs;
      Board.populate_board player1_board single_player_record.user;
      Board.populate_board player2_board single_player_record.cpu;
      cpu_attack_direction := single_player_record.cpu_direction;

      (* Populate queues *)
      Queue.clear cpu_horz_queue;
      queue_string_to_list single_player_record.cpu_horz_queue
      |> Queue.enqueue_all cpu_horz_queue;
      Queue.clear cpu_vert_queue;
      queue_string_to_list single_player_record.cpu_vert_queue
      |> Queue.enqueue_all cpu_vert_queue;
      false
