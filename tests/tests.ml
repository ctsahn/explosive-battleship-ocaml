open Core
open OUnit2
open Board
open Game

let test_board_to_string _ =
  assert_equal "0000"
  @@ board_to_string (Array.make_matrix ~dimx:2 ~dimy:2 Empty);
  assert_equal "1111"
  @@ board_to_string (Array.make_matrix ~dimx:2 ~dimy:2 Miss);
  assert_equal "2222"
  @@ board_to_string (Array.make_matrix ~dimx:2 ~dimy:2 Ship);
  assert_equal "3333"
  @@ board_to_string (Array.make_matrix ~dimx:2 ~dimy:2 ShipHit);
  assert_equal "4444"
  @@ board_to_string (Array.make_matrix ~dimx:2 ~dimy:2 ShipSunken)

let test_initialize_board _ =
  assert_equal
    ( Array.make_matrix ~dimx:10 ~dimy:10 Empty,
      Array.make_matrix ~dimx:10 ~dimy:10 Empty )
  @@ initialize_boards

let test_convert_position _ =
  assert_equal (0, 0) @@ convert_position 0;
  assert_equal (0, 1) @@ convert_position 1;
  assert_equal (9, 2) @@ convert_position 92

let board_tests =
  "Board"
  >: test_list
       [
         "board to string" >:: test_board_to_string;
         "initialize board" >:: test_initialize_board;
         "convert position" >:: test_convert_position;
       ]

let vertical_ship_sunk =
  let arr = Array.make_matrix ~dimx:3 ~dimy:3 Empty in
  arr.(1).(1) <- ShipHit;
  arr.(0).(1) <- ShipHit;
  arr

let vertical_ship_sunk_2 =
  let arr = Array.make_matrix ~dimx:2 ~dimy:2 Empty in
  arr.(1).(1) <- ShipHit;
  arr.(0).(1) <- ShipHit;
  arr

let vertical_ship_not_sunk =
  let arr = Array.make_matrix ~dimx:3 ~dimy:3 Ship in
  arr.(1).(1) <- ShipHit;
  arr.(0).(1) <- ShipHit;
  arr

let horizontal_ship_sunk =
  let arr = Array.make_matrix ~dimx:3 ~dimy:3 Miss in
  arr.(0).(1) <- ShipHit;
  arr.(0).(0) <- ShipHit;
  arr

let horizontal_ship_not_sunk =
  let arr = Array.make_matrix ~dimx:3 ~dimy:3 Ship in
  arr.(0).(1) <- ShipHit;
  arr.(0).(0) <- ShipHit;
  arr

let test_has_sunk _ =
  assert_equal true @@ has_sunk vertical_ship_sunk 0 1;
  assert_equal ShipSunken @@ vertical_ship_sunk.(0).(1);
  assert_equal ShipSunken @@ vertical_ship_sunk.(1).(1);

  assert_equal true @@ has_sunk vertical_ship_sunk_2 1 1;

  assert_equal false @@ has_sunk vertical_ship_not_sunk 1 1;
  assert_equal true @@ has_sunk horizontal_ship_sunk 0 1;
  assert_equal false @@ has_sunk horizontal_ship_not_sunk 0 0

let make_sample_array =
  let arr = Array.make_matrix ~dimx:2 ~dimy:2 Ship in
  arr.(1).(1) <- ShipSunken;
  arr.(0).(1) <- ShipHit;
  arr

let test_is_game_over _ =
  assert_equal true @@ is_game_over (Array.make_matrix ~dimx:2 ~dimy:2 Empty);
  assert_equal true
  @@ is_game_over (Array.make_matrix ~dimx:2 ~dimy:2 ShipSunken);
  assert_equal false @@ is_game_over make_sample_array

let empty_array =
  Array.make_matrix ~dimx:10 ~dimy:10 Empty

let ship_full_array =
  Array.make_matrix ~dimx:10 ~dimy:10 Ship

let test_attack _ =
  assert_equal true @@ attack ship_full_array 4;
  assert_equal false @@ attack empty_array 4

let test_cpu_attack _ =
  assert_equal false @@ cpu_attack empty_array;
  assert_equal true @@ Queue.is_empty cpu_horz_queue;
  assert_equal true @@ Queue.is_empty cpu_vert_queue;
  assert_equal true @@ cpu_attack ship_full_array;
  assert_equal "" @@ !cpu_attack_direction;
  assert_equal false @@ Queue.is_empty cpu_horz_queue;
  assert_equal false @@ Queue.is_empty cpu_vert_queue;
  assert_equal true @@ cpu_attack ship_full_array;
  assert_equal true
  @@ (String.( = ) !cpu_attack_direction "horizontal"
     || String.( = ) !cpu_attack_direction "vertical")

let test_place_ship _ =
  let arr = Array.make_matrix ~dimx:10 ~dimy:10 Empty in

  assert_equal true @@ place_ship arr 15 17;
  assert_equal Ship @@ arr.(1).(5);
  assert_equal Ship @@ arr.(1).(6);
  assert_equal Ship @@ arr.(1).(7);
  assert_equal true @@ place_ship arr 28 58;
  assert_equal Ship @@ arr.(2).(8);
  assert_equal Ship @@ arr.(3).(8);
  assert_equal Ship @@ arr.(4).(8);
  assert_equal Ship @@ arr.(5).(8)

let game_tests =
  "Game"
  >: test_list
       [
         "has_sunk" >:: test_has_sunk;
         "is_game_over" >:: test_is_game_over;
         "cpu_attack" >:: test_cpu_attack;
         "attack" >:: test_attack;
         "place_ship" >:: test_place_ship;
       ]

let series = "Battleship Tests" >::: [ board_tests; game_tests ]
let () = run_test_tt_main series
