open Core
open OUnit2
open Board

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
  @@ board_to_string (Array.make_matrix ~dimx:2 ~dimy:2 ShipSunken);
  assert_equal "5555"
  @@ board_to_string (Array.make_matrix ~dimx:2 ~dimy:2 Mine);
  assert_equal "6666"
  @@ board_to_string (Array.make_matrix ~dimx:2 ~dimy:2 MineHit)

let test_populate_board _ =
  let empty_arr = Array.make_matrix ~dimx:3 ~dimy:3 Empty in
  populate_board empty_arr "111111111";
  assert_equal (Array.make_matrix ~dimx:3 ~dimy:3 Miss) @@ empty_arr;

  let result_arr = Array.make_matrix ~dimx:3 ~dimy:3 Empty in
  result_arr.(0).(0) <- ShipHit;
  result_arr.(0).(1) <- ShipSunken;
  result_arr.(0).(2) <- Miss;
  result_arr.(1).(0) <- Ship;

  let empty_arr_2 = Array.make_matrix ~dimx:3 ~dimy:3 Empty in

  populate_board empty_arr_2 "341200000";
  assert_equal result_arr @@ empty_arr_2

let test_initialize_board _ =
  assert_equal
    ( Array.make_matrix ~dimx:10 ~dimy:10 Empty,
      Array.make_matrix ~dimx:10 ~dimy:10 Empty )
  @@ initialize_boards

let test_convert_position _ =
  assert_equal (0, 0) @@ convert_position 0;
  assert_equal (0, 1) @@ convert_position 1;
  assert_equal (9, 2) @@ convert_position 92

let test_reset _ =
  let sample_array =
    let arr = Array.make_matrix ~dimx:10 ~dimy:10 Ship in
    arr
  in
  reset sample_array;
  assert_equal (Array.make_matrix ~dimx:10 ~dimy:10 Empty) @@ sample_array

let board_tests =
  "Board"
  >: test_list
       [
         "board to string" >:: test_board_to_string;
         "populate_board" >:: test_populate_board;
         "initialize board" >:: test_initialize_board;
         "convert position" >:: test_convert_position;
         "reset" >:: test_reset;
       ]

let test_player_attack _ =
  let arr = Array.make_matrix ~dimx:10 ~dimy:10 Empty in

  arr.(4).(4) <- Empty;
  arr.(4).(5) <- Ship;
  arr.(4).(6) <- Mine;
  arr.(4).(7) <- Mine;
  arr.(4).(8) <- Mine;

  let penalty_arr = Array.make_matrix ~dimx:10 ~dimy:10 Empty in

  penalty_arr.(1).(1) <- Ship;
  penalty_arr.(1).(2) <- Ship;

  assert_equal false @@ Player.player_attack arr arr 4 4 false;
  assert_equal true @@ Player.player_attack arr arr 4 5 false;
  assert_equal false @@ Player.player_attack arr penalty_arr 4 6 false;
  assert_equal false @@ Player.player_attack arr penalty_arr 4 7 false;
  assert_equal ShipSunken @@ penalty_arr.(1).(1);
  assert_equal ShipSunken @@ penalty_arr.(1).(2);

  penalty_arr.(3).(5) <- Ship;
  penalty_arr.(3).(6) <- Ship;
  assert_equal false @@ Player.player_attack arr penalty_arr 4 8 true;

  assert_equal true
  @@ (equal_status penalty_arr.(3).(5) ShipHit
     || equal_status penalty_arr.(3).(6) ShipHit);
  assert_equal false @@ Queue.is_empty Cpu.horz_attack_queue;
  assert_equal false @@ Queue.is_empty Cpu.vert_attack_queue

let test_player_bomb _ =
  let empty_array = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  let arr = Array.make_matrix ~dimx:3 ~dimy:3 Empty in

  let res = Player.player_bomb arr empty_array 1 1 false in

  assert_equal false @@ res;
  assert_equal 9
  @@ Array.fold arr ~init:0 ~f:(fun tot arr_row ->
         tot + Array.count arr_row ~f:(fun e -> equal_status e Miss));

  let arr2 = Array.make_matrix ~dimx:3 ~dimy:3 Ship in
  let res2 = Player.player_bomb arr2 empty_array 1 1 false in
  assert_equal true @@ res2;
  assert_equal 9
  @@ Array.fold arr2 ~init:0 ~f:(fun tot arr_row ->
         tot + Array.count arr_row ~f:(fun e -> equal_status e ShipSunken))

let player_tests =
  "Player"
  >: test_list
       [
         "player_attack" >:: test_player_attack;
         "player_bomb" >:: test_player_bomb;
       ]

let test_has_sunk _ =
  let vertical_ship_sunk =
    let arr = Array.make_matrix ~dimx:3 ~dimy:3 Empty in
    arr.(1).(1) <- ShipHit;
    arr.(0).(1) <- ShipHit;
    arr
  in

  let vertical_ship_sunk_2 =
    let arr = Array.make_matrix ~dimx:2 ~dimy:2 Empty in
    arr.(1).(1) <- ShipHit;
    arr.(0).(1) <- ShipHit;
    arr
  in

  let vertical_ship_not_sunk =
    let arr = Array.make_matrix ~dimx:3 ~dimy:3 Ship in
    arr.(1).(1) <- ShipHit;
    arr.(0).(1) <- ShipHit;
    arr
  in

  let vertical_ship_not_sunk_2 = 
    let arr = Array.make_matrix ~dimx:3 ~dimy:3 Miss in 
    arr.(0).(1) <- Ship;
    arr.(1).(1) <- ShipHit;
    arr
  in

  let vertical_ship_not_sunk_3 = 
    let arr = Array.make_matrix ~dimx:3 ~dimy:3 Miss in 
    arr.(0).(0) <- ShipHit;
    arr.(1).(0) <- Ship;
    arr
  in

  let horizontal_ship_sunk =
    let arr = Array.make_matrix ~dimx:3 ~dimy:3 Miss in
    arr.(0).(1) <- ShipHit;
    arr.(0).(0) <- ShipHit;
    arr
  in

  let horizontal_ship_not_sunk =
    let arr = Array.make_matrix ~dimx:3 ~dimy:3 Ship in
    arr.(0).(1) <- ShipHit;
    arr.(0).(0) <- ShipHit;
    arr
  in

  assert_equal true @@ Game.has_sunk vertical_ship_sunk 0 1;
  assert_equal ShipSunken @@ vertical_ship_sunk.(0).(1);
  assert_equal ShipSunken @@ vertical_ship_sunk.(1).(1);

  assert_equal true @@ Game.has_sunk vertical_ship_sunk_2 1 1;

  assert_equal false @@ Game.has_sunk vertical_ship_not_sunk 0 0;
  assert_equal false @@ Game.has_sunk vertical_ship_not_sunk 0 1;
  assert_equal false @@ Game.has_sunk vertical_ship_not_sunk 1 2;
  assert_equal false @@ Game.has_sunk vertical_ship_not_sunk 0 2;

  assert_equal false @@ Game.has_sunk vertical_ship_not_sunk_2 1 1;
  assert_equal false @@ Game.has_sunk vertical_ship_not_sunk_3 0 0;

  assert_equal false @@ Game.has_sunk vertical_ship_not_sunk 1 1;
  assert_equal true @@ Game.has_sunk horizontal_ship_sunk 0 1;
  assert_equal false @@ Game.has_sunk horizontal_ship_not_sunk 0 0

let test_find_ship _ =
  let arr = Array.make_matrix ~dimx:3 ~dimy:3 Empty in
  arr.(1).(1) <- Ship;
  assert_equal (1, 1) @@ Game.find_ship arr

let test_is_game_over _ =
  let make_sample_array =
    let arr = Array.make_matrix ~dimx:2 ~dimy:2 Ship in
    arr.(1).(1) <- ShipSunken;
    arr.(0).(1) <- ShipHit;
    arr
  in
  assert_equal true
  @@ Game.is_game_over (Array.make_matrix ~dimx:2 ~dimy:2 Empty);

  assert_equal true
  @@ Game.is_game_over (Array.make_matrix ~dimx:2 ~dimy:2 ShipSunken);

  assert_equal false @@ Game.is_game_over make_sample_array

let test_place_ship _ =
  let arr = Array.make_matrix ~dimx:10 ~dimy:10 Empty in

  assert_equal (Ok 3) @@ Game.place_ship arr "" 1 5 1 7;
  assert_equal Ship @@ arr.(1).(5);
  assert_equal Ship @@ arr.(1).(6);
  assert_equal Ship @@ arr.(1).(7);
  assert_equal Miss @@ arr.(1).(4);
  assert_equal Miss @@ arr.(1).(8);

  assert_equal Miss @@ arr.(0).(4);
  assert_equal Miss @@ arr.(0).(5);
  assert_equal Miss @@ arr.(0).(6);
  assert_equal Miss @@ arr.(0).(7);
  assert_equal Miss @@ arr.(0).(8);

  assert_equal Miss @@ arr.(2).(4);
  assert_equal Miss @@ arr.(2).(5);
  assert_equal Miss @@ arr.(2).(6);
  assert_equal Miss @@ arr.(2).(7);
  assert_equal Miss @@ arr.(2).(8);

  assert_equal (Ok 4) @@ Game.place_ship arr "" 2 9 5 9;
  assert_equal Ship @@ arr.(2).(9);
  assert_equal Ship @@ arr.(3).(9);
  assert_equal Ship @@ arr.(4).(9);
  assert_equal Ship @@ arr.(5).(9);

  assert_equal Miss @@ arr.(1).(9);
  assert_equal Miss @@ arr.(6).(9);

  assert_equal Miss @@ arr.(1).(8);
  assert_equal Miss @@ arr.(6).(8);

  assert_equal Miss @@ arr.(2).(8);
  assert_equal Miss @@ arr.(3).(8);
  assert_equal Miss @@ arr.(4).(8);
  assert_equal Miss @@ arr.(5).(8);

  assert_equal (Ok 2) @@ Game.place_ship arr "" 4 7 4 6;
  assert_equal Ship @@ arr.(4).(6);
  assert_equal Ship @@ arr.(4).(7);

  assert_equal Miss @@ arr.(3).(7);
  assert_equal Miss @@ arr.(3).(6);
  assert_equal Miss @@ arr.(5).(6);
  assert_equal Miss @@ arr.(5).(7);
  assert_equal Miss @@ arr.(4).(8);
  assert_equal Miss @@ arr.(4).(5);

  assert_equal Miss @@ arr.(5).(8);
  assert_equal Miss @@ arr.(3).(8);
  assert_equal Miss @@ arr.(5).(5);
  assert_equal Miss @@ arr.(3).(5);

  assert_equal (Ok 1) @@ Game.place_ship arr "" 3 3 3 3;
  assert_equal Mine @@ arr.(3).(3);

  assert_equal Miss @@ arr.(3).(2);
  assert_equal Miss @@ arr.(3).(4);
  assert_equal Miss @@ arr.(2).(3);
  assert_equal Miss @@ arr.(4).(3);
  assert_equal Miss @@ arr.(4).(4);
  assert_equal Miss @@ arr.(2).(4);
  assert_equal Miss @@ arr.(2).(2);
  assert_equal Miss @@ arr.(4).(2);

  assert_equal (Error Game.not_straight_error) @@ Game.place_ship arr "" 7 8 8 9;
  assert_equal (Error Game.not_straight_error) @@ Game.place_ship arr "" 4 7 7 6;
  assert_equal (Error Game.touching_error) @@ Game.place_ship arr "" 4 7 4 6;
  assert_equal (Error Game.touching_error) @@ Game.place_ship arr "" 3 3 2 3;
  assert_equal (Error Game.too_big_error) @@ Game.place_ship arr "" 0 0 6 0;
  assert_equal (Error Game.too_big_error) @@ Game.place_ship arr "" 7 0 7 6;
  assert_equal (Error Game.repeat_error) @@ Game.place_ship arr "4" 0 0 3 0;
  assert_equal (Error Game.repeat_error) @@ Game.place_ship arr "2" 0 1 0 2;
  assert_equal (Error Game.touching_error) @@ Game.place_ship arr "" 4 7 5 7
  

let test_is_valid_attack _ =
  let arr = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  arr.(1).(1) <- Miss;
  arr.(2).(1) <- Ship;
  arr.(3).(1) <- ShipHit;
  arr.(4).(1) <- ShipSunken;
  arr.(5).(1) <- Mine;
  arr.(6).(1) <- MineHit;

  assert_equal true @@ Game.is_valid_attack arr 0 1;
  assert_equal false @@ Game.is_valid_attack arr 1 1;
  assert_equal true @@ Game.is_valid_attack arr 2 1;
  assert_equal false @@ Game.is_valid_attack arr 3 1;
  assert_equal false @@ Game.is_valid_attack arr 4 1;
  assert_equal true @@ Game.is_valid_attack arr 5 1;
  assert_equal false @@ Game.is_valid_attack arr 6 1

let test_cleanse_board _ =
  let arr = Array.make_matrix ~dimx:10 ~dimy:10 Empty in

  assert_equal (Ok 3) @@ Game.place_ship arr "" 1 5 1 7;
  Game.cleanse_board arr;
  assert_equal Ship @@ arr.(1).(5);
  assert_equal Ship @@ arr.(1).(6);
  assert_equal Ship @@ arr.(1).(7);

  assert_equal Empty @@ arr.(1).(4);
  assert_equal Empty @@ arr.(1).(8);

  assert_equal Empty @@ arr.(0).(4);
  assert_equal Empty @@ arr.(0).(5);
  assert_equal Empty @@ arr.(0).(6);
  assert_equal Empty @@ arr.(0).(7);
  assert_equal Empty @@ arr.(0).(8);

  assert_equal Empty @@ arr.(2).(4);
  assert_equal Empty @@ arr.(2).(5);
  assert_equal Empty @@ arr.(2).(6);
  assert_equal Empty @@ arr.(2).(7);
  assert_equal Empty @@ arr.(2).(8)

let test_save_load_two_player_game _ =
  let player1_board = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  let _ = Game.place_ship player1_board "" 1 5 2 5 in
  player1_board.(9).(9) <- Mine;
  let player2_board = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  let _ = Game.place_ship player2_board "" 6 1 8 1 in
  player2_board.(9).(9) <- MineHit;
  Game.save_two_player_game player1_board player2_board 3 2 "player1";

  let new_player1_board = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  let new_player2_board = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  let loaded_player1_bombs = ref 0 in
  let loaded_player2_bombs = ref 0 in
  let loaded_turn = ref "" in
  let res =
    Game.load_game new_player1_board new_player2_board loaded_player1_bombs
      loaded_player2_bombs loaded_turn (Queue.create ()) (Queue.create ())
      (ref "")
  in

  assert_equal true @@ res;
  assert_equal player1_board @@ new_player1_board;
  assert_equal player2_board @@ new_player2_board;
  assert_equal 3 @@ !loaded_player1_bombs;
  assert_equal 2 @@ !loaded_player2_bombs;
  assert_equal "player1" @@ !loaded_turn

let test_save_load_single_player_game _ =
  let user_board = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  let _ = Game.place_ship user_board "" 1 5 2 5 in
  let cpu_board = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  let _ = Game.place_ship cpu_board "" 6 1 8 1 in

  let horz_queue = Queue.create () in
  Queue.enqueue horz_queue (5, 5);
  Queue.enqueue horz_queue (5, 7);

  let vert_queue = Queue.create () in
  Queue.enqueue vert_queue (8, 8);
  Queue.enqueue vert_queue (6, 8);

  Game.save_single_player_game user_board cpu_board 3 2 "user" horz_queue
    vert_queue "vertical";

  let new_user_board = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  let new_cpu_board = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  let new_horz_queue = Queue.create () in
  let new_vert_queue = Queue.create () in

  let loaded_turn = ref "" in
  let loaded_dir = ref "" in
  let loaded_user_bombs = ref 0 in
  let loaded_cpu_bombs = ref 0 in
  let res =
    Game.load_game new_user_board new_cpu_board loaded_user_bombs
      loaded_cpu_bombs loaded_turn new_horz_queue new_vert_queue loaded_dir
  in

  assert_equal false @@ res;
  assert_equal user_board @@ new_user_board;
  assert_equal cpu_board @@ new_cpu_board;
  assert_equal "user" @@ !loaded_turn;
  assert_equal (Queue.to_list horz_queue) @@ Queue.to_list new_horz_queue;
  assert_equal (Queue.to_list vert_queue) @@ Queue.to_list new_vert_queue;
  assert_equal "vertical" @@ !loaded_dir;


  Queue.clear horz_queue;
  Queue.clear vert_queue;

  Game.save_single_player_game user_board cpu_board 0 0 "cpu" horz_queue
    vert_queue "horizontal";


    let new_user_board_2 = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  let new_cpu_board_2 = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  let new_horz_queue_2 = Queue.create () in
  let new_vert_queue_2 = Queue.create () in

  let loaded_turn_2 = ref "" in
  let loaded_dir_2 = ref "" in
  let loaded_user_bombs_2 = ref 0 in
  let loaded_cpu_bombs_2 = ref 0 in
  let res_2 =
    Game.load_game new_user_board_2 new_cpu_board_2 loaded_user_bombs_2
      loaded_cpu_bombs_2 loaded_turn_2 new_horz_queue_2 new_vert_queue_2 loaded_dir_2
  in

  assert_equal false @@ res_2;
  assert_equal user_board @@ new_user_board_2;
  assert_equal cpu_board @@ new_cpu_board_2;
  assert_equal "cpu" @@ !loaded_turn_2;
  assert_equal (Queue.to_list horz_queue) @@ Queue.to_list new_horz_queue_2;
  assert_equal (Queue.to_list vert_queue) @@ Queue.to_list new_vert_queue_2;
  assert_equal "horizontal" @@ !loaded_dir_2



let game_tests =
  "Game"
  >: test_list
       [
         "has_sunk" >:: test_has_sunk;
         "find_ship" >:: test_find_ship;
         "is_game_over" >:: test_is_game_over;
         "place_ship" >:: test_place_ship;
         "is_valid_attack" >:: test_is_valid_attack;
         "cleanse_board" >:: test_cleanse_board;
         "save_two_player_game, load_game" >:: test_save_load_two_player_game;
         "save_single_player_game, load_game"
         >:: test_save_load_single_player_game;
       ]

let test_cpu_attack _ =
  Queue.clear Cpu.horz_attack_queue;
  Queue.clear Cpu.vert_attack_queue;
  let empty_array = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  let ship_full_array = Array.make_matrix ~dimx:10 ~dimy:10 Ship in

  assert_equal false @@ Cpu.cpu_attack empty_array empty_array (ref 0);
  assert_equal true @@ Queue.is_empty Cpu.horz_attack_queue;
  assert_equal true @@ Queue.is_empty Cpu.vert_attack_queue;

  assert_equal true @@ Cpu.cpu_attack ship_full_array empty_array (ref 0);
  assert_equal "" @@ !Cpu.attack_direction;
  assert_equal false @@ Queue.is_empty Cpu.horz_attack_queue;
  assert_equal false @@ Queue.is_empty Cpu.vert_attack_queue;
  assert_equal true @@ Cpu.cpu_attack ship_full_array empty_array (ref 0);
  assert_equal true
  @@ (String.( = ) !Cpu.attack_direction "horizontal"
     || String.( = ) !Cpu.attack_direction "vertical");
  assert_equal true @@ Cpu.cpu_attack ship_full_array empty_array (ref 0);
  assert_equal true
  @@ (String.( = ) !Cpu.attack_direction "horizontal"
     || String.( = ) !Cpu.attack_direction "vertical");

  Queue.clear Cpu.horz_attack_queue;
  Queue.clear Cpu.vert_attack_queue;

  (* Will use bombs here *)

  Array.fill empty_array.(0) ~pos:0 ~len:10 Miss;
  Array.fill empty_array.(9) ~pos:0 ~len:10 Miss;
  Array.iter empty_array ~f: (fun row -> row.(0) <- Miss;row.(9) <- Miss; );

  assert_equal false @@ Cpu.cpu_attack empty_array empty_array (ref 1);


  Array.fill ship_full_array.(0) ~pos:0 ~len:10 Miss;
  Array.fill ship_full_array.(9) ~pos:0 ~len:10 Miss;
  Array.iter ship_full_array ~f: (fun row -> row.(0) <- Miss;row.(9) <- Miss; );
  assert_equal true @@ Cpu.cpu_attack ship_full_array empty_array (ref 1);

  Queue.clear Cpu.horz_attack_queue;
  Queue.clear Cpu.vert_attack_queue;

  let hit_full_array =  Array.make_matrix ~dimx:10 ~dimy:10 ShipHit in 
  hit_full_array.(5).(5) <- Empty; 

  assert_equal false @@ Cpu.cpu_attack hit_full_array empty_array (ref 0);
  assert_equal Miss @@ hit_full_array.(5).(5);

  Queue.enqueue Cpu.horz_attack_queue (6,6); 

  let arr = Array.make_matrix ~dimx:10 ~dimy:10 Empty in 

  assert_equal false @@ Cpu.cpu_attack arr empty_array (ref 0 ); 
  assert_equal Miss @@ arr.(6).(6);


  Queue.clear Cpu.horz_attack_queue;
  Queue.enqueue Cpu.vert_attack_queue (7,7); 
  assert_equal false @@ Cpu.cpu_attack arr empty_array (ref 0 ); 
  assert_equal Miss @@ arr.(7).(7);

  Queue.clear Cpu.vert_attack_queue; 

  Queue.enqueue Cpu.horz_attack_queue (8,8); 
  Queue.enqueue Cpu.vert_attack_queue (9,9);
  Cpu.attack_direction := "vertical";  
  assert_equal false @@ Cpu.cpu_attack arr empty_array (ref 0 ); 
  assert_equal Miss @@ arr.(9).(9);
  assert_equal true @@ Queue.is_empty Cpu.vert_attack_queue;

  Queue.enqueue Cpu.vert_attack_queue (3,9);
  Cpu.attack_direction := "horizontal";  
  assert_equal false @@ Cpu.cpu_attack arr empty_array (ref 0 ); 
  assert_equal Miss @@ arr.(8).(8);
  assert_equal true @@ Queue.is_empty Cpu.horz_attack_queue;

  Queue.enqueue Cpu.horz_attack_queue (4,9);
  Cpu.attack_direction := "";

  assert_equal false @@ Cpu.cpu_attack arr empty_array (ref 0 ); 
  assert_equal true @@ (equal_status arr.(4).(9) Miss ||equal_status arr.(3).(9) Miss ) ;
  assert_equal true @@ (Queue.is_empty Cpu.horz_attack_queue || Queue.is_empty Cpu.vert_attack_queue)


let test_attack_given_coords _ =
  Queue.clear Cpu.horz_attack_queue;
  Queue.clear Cpu.vert_attack_queue;
  let arr = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  arr.(1).(1) <- Ship;
  arr.(1).(2) <- Ship;
  arr.(4).(4) <- Mine;
  arr.(4).(5) <- Mine;

  let penalty_arr = Array.make_matrix ~dimx:10 ~dimy:10 Empty in

  penalty_arr.(3).(3) <- Ship;
  penalty_arr.(3).(4) <- Ship;
  assert_equal false @@ Cpu.attack_given_coords arr penalty_arr 1 3;
  assert_equal true @@ Cpu.attack_given_coords arr penalty_arr 1 1;
  assert_equal true @@ Cpu.attack_given_coords arr penalty_arr 1 2;
  assert_equal true
  @@ (Queue.is_empty Cpu.vert_attack_queue
     && Queue.is_empty Cpu.horz_attack_queue);
  assert_equal false @@ Cpu.attack_given_coords arr penalty_arr 4 4;
  assert_equal MineHit @@ arr.(4).(4);
  assert_equal false @@ Cpu.attack_given_coords arr penalty_arr 4 5;
  assert_equal MineHit @@ arr.(4).(5);
  assert_equal ShipSunken @@ penalty_arr.(3).(3);
  assert_equal ShipSunken @@ penalty_arr.(3).(4)

let test_place_cpu_ships _ =
  let arr = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  Cpu.place_cpu_ships arr 5;
  assert_equal (5 + 4 + 3 + 2)
  @@ Array.fold arr ~init:0 ~f:(fun tot arr_row ->
         tot + Array.count arr_row ~f:(fun e -> equal_status e Ship));
  let arr2 = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  Cpu.place_cpu_ships arr2 4;
  assert_equal (4 + 3 + 2)
  @@ Array.fold arr2 ~init:0 ~f:(fun tot arr_row ->
         tot + Array.count arr_row ~f:(fun e -> equal_status e Ship));

  let arr3 = Array.make_matrix ~dimx:10 ~dimy:10 Empty in
  Cpu.place_cpu_ships arr3 3;
  assert_equal (3 + 2)
  @@ Array.fold arr3 ~init:0 ~f:(fun tot arr_row ->
         tot + Array.count arr_row ~f:(fun e -> equal_status e Ship));

  let arr4 = Array.make_matrix ~dimx:10 ~dimy:10 Ship in
  arr4.(1).(1) <- Empty;

  arr4.(1).(2) <- Empty;

  arr4.(4).(4) <- Empty;

  Cpu.place_cpu_ships arr4 2;

  assert_equal 99
  @@ Array.fold arr4 ~init:0 ~f:(fun tot arr_row ->
         tot + Array.count arr_row ~f:(fun e -> equal_status e Ship));

  assert_equal Mine @@ arr4.(4).(4)

let cpu_tests =
  "CPU"
  >: test_list
       [
         "cpu_attack" >:: test_cpu_attack;
         "attack_given_coords" >:: test_attack_given_coords;
         "place_cpu_ships" >:: test_place_cpu_ships;
       ]

let series =
  "Battleship Tests" >::: [ board_tests; game_tests; player_tests; cpu_tests ]

let () = run_test_tt_main series
