open Core;;
open OUnit2;;
open Board;;

let test_board_to_string _ =
  assert_equal "0000" @@ board_to_string (Array.make_matrix ~dimx:2 ~dimy:2 Empty);
  assert_equal "1111" @@ board_to_string (Array.make_matrix ~dimx:2 ~dimy:2 Miss);
  assert_equal "2222" @@ board_to_string (Array.make_matrix ~dimx:2 ~dimy:2 Ship);
  assert_equal "3333" @@ board_to_string (Array.make_matrix ~dimx:2 ~dimy:2 ShipHit);
  assert_equal "4444" @@ board_to_string (Array.make_matrix ~dimx:2 ~dimy:2 ShipSunken)

let test_initialize_board _ = 
  assert_equal (Array.make_matrix ~dimx:10 ~dimy:10 Empty) @@ initialize_board

let test_convert_position _ =
  assert_equal (0, 0) @@ convert_position 0;
  assert_equal (0, 1) @@ convert_position 10;
  assert_equal (9, 2) @@ convert_position 29

let board_tests = "Board" >: test_list [
  "board to string" >:: test_board_to_string;
  "initialize board" >:: test_initialize_board;
  "convert position" >:: test_convert_position;
]

let series = "Battleship Tests" >::: [
  board_tests;
]

let () = 
  run_test_tt_main series