open Core

let boards = Board.initialize_boards
let player1_board = fst boards (* User's board when single player *)
let player2_board = snd boards (* CPU's board when single player *)
let current_turn = ref ""
let player1_bombs = ref 3
let player2_bombs = ref 3
let ship_placed = ref false (* true after two clicks *)
let click1 = ref 0
let click2 = ref 0
let player1_ship_status = ref ""
(* User placement of ships - which ship lengths have been placed so far *)

let player2_ship_status = ref ""
(* User placement of ships - which ship lengths have been placed so far *)

let ship_size = ref ""

let handle_ship_placement (player_board : Board.t)
    (player_ship_status : string ref) (user_move : string) request =
  if !ship_placed then (
    click2 := Int.of_string user_move;
    ship_placed := false;
    let click1_coords = Board.convert_position !click1 in
    let click2_coords = Board.convert_position !click2 in

    let row1 = fst click1_coords in
    let col1 = snd click1_coords in
    let row2 = fst click2_coords in
    let col2 = snd click2_coords in
    match
      Game.place_ship player_board !player_ship_status row1 col1 row2 col2
    with
    (* A valid placement of the ship *)
    | Ok v ->
        ship_size := Int.to_string v;

        player_ship_status := !player_ship_status ^ !ship_size;

        let sorted_ship_sizes =
          List.sort (String.to_list !player_ship_status) ~compare:Char.compare
        in
        let expected_ship_sizes = [ '1'; '2'; '3'; '4'; '5' ] in

        if List.equal Char.equal sorted_ship_sizes expected_ship_sizes then
          Dream.html
            (Template.ship_placement ~turn:!current_turn
               ~user_board_status:(Board.board_to_string player_board)
               ~ship_status:!player_ship_status ~placed_ship_size:!ship_size
               ~ready:"true" ~error:"" request)
        else
          Dream.html
            (Template.ship_placement ~turn:!current_turn
               ~user_board_status:(Board.board_to_string player_board)
               ~ship_status:!player_ship_status ~placed_ship_size:!ship_size
               ~ready:"false" ~error:"" request)
    | Error e ->
        (* Not valid placement, alert user of error *)
        Dream.html
          (Template.ship_placement ~turn:!current_turn
             ~user_board_status:(Board.board_to_string player_board)
             ~ship_status:!player_ship_status ~placed_ship_size:!ship_size
             ~ready:"false" ~error:e request))
  else (
    click1 := Int.of_string user_move;
    ship_placed := true;
    ship_size := "";
    Dream.html
      (Template.ship_placement ~turn:!current_turn
         ~user_board_status:(Board.board_to_string player_board)
         ~ship_status:!player_ship_status ~placed_ship_size:!ship_size
         ~ready:"false" ~error:"" request))

let handle_placement_reset request =
  ship_placed := false;
  click1 := 0;
  click2 := 0;
  if String.( = ) !current_turn "player1" || String.( = ) !current_turn "user"
  then (
    Board.reset player1_board;
    player1_ship_status := "";
    Dream.html
      (Template.ship_placement ~turn:!current_turn
         ~user_board_status:(Board.board_to_string player1_board)
         ~ship_status:!player1_ship_status ~placed_ship_size:"0" ~ready:"false"
         ~error:"" request))
  else (
    Board.reset player2_board;
    player2_ship_status := "";
    Dream.html
      (Template.ship_placement ~turn:!current_turn
         ~user_board_status:(Board.board_to_string player2_board)
         ~ship_status:!player2_ship_status ~placed_ship_size:"0" ~ready:"false"
         ~error:"" request))

type two_player_save = { player1 : string; player2 : string; turn : string }
[@@deriving yojson { exn = true }]

type single_player_save = {
  user : string;
  cpu : string;
  turn : string;
  cpu_horz_queue : string;
  cpu_vert_queue : string;
  cpu_direction : string;
}
[@@deriving yojson { exn = true }]

let handle_save request =
  (* Save a single player game *)
  if String.( = ) !current_turn "user" || String.( = ) !current_turn "cpu" then (
    Game.save_single_player_game player1_board player2_board !player1_bombs
      !player2_bombs !current_turn Cpu.horz_attack_queue Cpu.vert_attack_queue
      !Cpu.attack_direction;

    Dream.html
      (Template.single_player_game_board
         ~user_board_status:(Board.board_to_string player1_board)
         ~cpu_board_status:(Board.board_to_string player2_board)
         ~user_bombs:(Int.to_string !player1_bombs)
         ~cpu_bombs:(Int.to_string !player2_bombs)
         ~turn:!current_turn ~game_over:"false" request))
  else (
    Game.save_two_player_game player1_board player2_board !player1_bombs
      !player2_bombs !current_turn;
    Dream.html
      (Template.two_player_game_board
         ~player1_board_status:(Board.board_to_string player1_board)
         ~player2_board_status:(Board.board_to_string player2_board)
         ~player1_bombs:(Int.to_string !player1_bombs)
         ~player2_bombs:(Int.to_string !player2_bombs)
         ~turn:!current_turn ~game_over:"false" request))

let handle_load request =
  let load_result =
    Game.load_game player1_board player2_board player1_bombs player2_bombs
      current_turn Cpu.horz_attack_queue Cpu.vert_attack_queue
      Cpu.attack_direction
  in

  if load_result then
    Dream.html
      (Template.two_player_game_board
         ~player1_board_status:(Board.board_to_string player1_board)
         ~player2_board_status:(Board.board_to_string player2_board)
         ~player1_bombs:(Int.to_string !player1_bombs)
         ~player2_bombs:(Int.to_string !player2_bombs)
         ~turn:!current_turn ~game_over:"false" request)
  else
    Dream.html
      (Template.single_player_game_board
         ~user_board_status:(Board.board_to_string player1_board)
         ~cpu_board_status:(Board.board_to_string player2_board)
         ~user_bombs:(Int.to_string !player1_bombs)
         ~cpu_bombs:(Int.to_string !player2_bombs)
         ~turn:!current_turn ~game_over:"false" request)

(* For 2-player turns *)
let handle_player_turn (user_move : string) (opponent_board : Board.t)
    (penalty_board : Board.t) request =
  let ship_hit =
    if
      String.is_substring user_move ~substring:"bomb"
      && ((String.( = ) !current_turn "player1" && !player1_bombs > 0)
         || (String.( = ) !current_turn "player2" && !player2_bombs > 0))
    then (
      let user_move_tuple =
        Int.of_string (String.chop_prefix_exn user_move ~prefix:"bomb")
        |> Board.convert_position
      in
      let row = fst user_move_tuple in
      let col = snd user_move_tuple in

      if String.( = ) !current_turn "player1" then
        player1_bombs := !player1_bombs - 1
      else player2_bombs := !player2_bombs - 1;

      Player.use_bomb opponent_board penalty_board row col false)
    else
      let user_move_tuple = Int.of_string user_move |> Board.convert_position in

      (* If a ship was hit, true *)
      let row = fst user_move_tuple in
      let col = snd user_move_tuple in
      Player.player_attack opponent_board penalty_board row col false
  in

  if ship_hit then
    if Game.is_game_over opponent_board then
      Dream.html
        (Template.two_player_game_board
           ~player1_board_status:(Board.board_to_string player1_board)
           ~player2_board_status:(Board.board_to_string player2_board)
           ~player1_bombs:(Int.to_string !player1_bombs)
           ~player2_bombs:(Int.to_string !player2_bombs)
           ~turn:!current_turn ~game_over:"true" request)
    else
      Dream.html
        (Template.two_player_game_board
           ~player1_board_status:(Board.board_to_string player1_board)
           ~player2_board_status:(Board.board_to_string player2_board)
           ~player1_bombs:(Int.to_string !player1_bombs)
           ~player2_bombs:(Int.to_string !player2_bombs)
           ~turn:!current_turn ~game_over:"false" request)
  else (
    (* if miss, rotate turn *)
    current_turn :=
      if String.( = ) !current_turn "player1" then "player2" else "player1";

    Dream.html
      (Template.two_player_game_board
         ~player1_board_status:(Board.board_to_string player1_board)
         ~player2_board_status:(Board.board_to_string player2_board)
         ~player1_bombs:(Int.to_string !player1_bombs)
         ~player2_bombs:(Int.to_string !player2_bombs)
         ~turn:!current_turn ~game_over:"false" request))

let handle_cpu_turn request =
  let ship_hit = Cpu.cpu_attack player1_board player2_board player2_bombs in
  if ship_hit then
    if Game.is_game_over player1_board then
      Dream.html
        (Template.single_player_game_board
           ~user_board_status:(Board.board_to_string player1_board)
           ~cpu_board_status:(Board.board_to_string player2_board)
           ~user_bombs:(Int.to_string !player1_bombs)
           ~cpu_bombs:(Int.to_string !player2_bombs)
           ~turn:!current_turn ~game_over:"true" request)
    else
      Dream.html
        (Template.single_player_game_board
           ~user_board_status:(Board.board_to_string player1_board)
           ~cpu_board_status:(Board.board_to_string player2_board)
           ~user_bombs:(Int.to_string !player1_bombs)
           ~cpu_bombs:(Int.to_string !player2_bombs)
           ~turn:!current_turn ~game_over:"false" request)
  else (
    (* if miss, rotate turn *)
    current_turn := "user";
    Dream.html
      (Template.single_player_game_board
         ~user_board_status:(Board.board_to_string player1_board)
         ~cpu_board_status:(Board.board_to_string player2_board)
         ~user_bombs:(Int.to_string !player1_bombs)
         ~cpu_bombs:(Int.to_string !player2_bombs)
         ~turn:!current_turn ~game_over:"false" request))

(* For single player turn *)
let handle_user_turn (user_move : string) request =
  let ship_hit =
    if String.is_substring user_move ~substring:"bomb" && !player1_bombs > 0
    then (
      let user_move_tuple =
        Int.of_string (String.chop_prefix_exn user_move ~prefix:"bomb")
        |> Board.convert_position
      in
      let row = fst user_move_tuple in
      let col = snd user_move_tuple in

      player1_bombs := !player1_bombs - 1;

      Player.use_bomb player2_board player1_board row col true)
    else
      let user_move_tuple = Int.of_string user_move |> Board.convert_position in

      (* If a ship was hit, true *)
      let row = fst user_move_tuple in
      let col = snd user_move_tuple in
      Player.player_attack player2_board player1_board row col true
  in

  if ship_hit then
    if Game.is_game_over player2_board then
      Dream.html
        (Template.single_player_game_board
           ~user_board_status:(Board.board_to_string player1_board)
           ~cpu_board_status:(Board.board_to_string player2_board)
           ~user_bombs:(Int.to_string !player1_bombs)
           ~cpu_bombs:(Int.to_string !player2_bombs)
           ~turn:!current_turn ~game_over:"true" request)
    else
      Dream.html
        (Template.single_player_game_board
           ~user_board_status:(Board.board_to_string player1_board)
           ~cpu_board_status:(Board.board_to_string player2_board)
           ~user_bombs:(Int.to_string !player1_bombs)
           ~cpu_bombs:(Int.to_string !player2_bombs)
           ~turn:!current_turn ~game_over:"false" request)
  else (
    (* if miss, rotate turn *)
    current_turn := "cpu";
    Dream.html
      (Template.single_player_game_board
         ~user_board_status:(Board.board_to_string player1_board)
         ~cpu_board_status:(Board.board_to_string player2_board)
         ~user_bombs:(Int.to_string !player1_bombs)
         ~cpu_bombs:(Int.to_string !player2_bombs)
         ~turn:!current_turn ~game_over:"false" request))

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.router
       [
         (* start screen, choose game type *)
         Dream.get "/" (fun _ -> Dream.html Template.start_screen);
         (* initial ship placement screen - single player *)

         (* start screen, choose game type *)
         Dream.get "/load" (fun request -> handle_load request);
         Dream.get "/placement" (fun request ->
             current_turn := "user";
             Dream.html
               (Template.ship_placement ~turn:!current_turn
                  ~user_board_status:(Board.board_to_string player1_board)
                  ~ship_status:!player1_ship_status ~placed_ship_size:"0"
                  ~ready:"false" ~error:"" request));
         (* send position of one click for ship placement *)
         Dream.post "/placement" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("user-place", message) ] ->
                 handle_ship_placement player1_board player1_ship_status message
                   request
             | _ -> Dream.empty `Bad_Request);
         (* player1 placement screen - user vs. user *)
         Dream.get "/player1_placement" (fun request ->
             current_turn := "player1";
             Dream.html
               (Template.ship_placement ~turn:!current_turn
                  ~user_board_status:(Board.board_to_string player1_board)
                  ~ship_status:!player1_ship_status ~placed_ship_size:"0"
                  ~ready:"false" ~error:"" request));
         (* send info of one square for placement *)
         Dream.post "/player1_placement" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("user-place", message) ] ->
                 handle_ship_placement player1_board player1_ship_status message
                   request
             | _ -> Dream.empty `Bad_Request);
         (* player2 placement screen - user vs. user *)
         Dream.get "/player2_placement" (fun request ->
             current_turn := "player2";
             Dream.html
               (Template.ship_placement ~turn:!current_turn
                  ~user_board_status:(Board.board_to_string player2_board)
                  ~ship_status:!player2_ship_status ~placed_ship_size:"0"
                  ~ready:"false" ~error:"" request));
         (* send info of one square for placement *)
         Dream.post "/player2_placement" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("user-place", message) ] ->
                 handle_ship_placement player2_board player2_ship_status message
                   request
             | _ -> Dream.empty `Bad_Request);
         (* play game - 2-player *)
         Dream.get "/play_two_player" (fun request ->
             Game.cleanse_board player1_board;
             Game.cleanse_board player2_board;
             current_turn := "player1";
             Dream.html
               (Template.two_player_game_board
                  ~player1_board_status:(Board.board_to_string player1_board)
                  ~player2_board_status:(Board.board_to_string player2_board)
                  ~player1_bombs:(Int.to_string !player1_bombs)
                  ~player2_bombs:(Int.to_string !player2_bombs)
                  ~turn:!current_turn ~game_over:"false" request));
         (* Start out with user turn with a blank message/board status*)
         Dream.post "/player1_turn" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("player1-move", message) ] ->
                 Core_unix.sleep 1;
                 (* short delay so we can actually see the move being made *)
                 handle_player_turn message player2_board player1_board request
             | _ -> Dream.empty `Bad_Request);
         Dream.post "/player2_turn" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("player2-move", message) ] ->
                 Core_unix.sleep 1;
                 (* short delay so we can actually see the move being made *)
                 handle_player_turn message player1_board player2_board request
             | _ -> Dream.empty `Bad_Request);
         (* play game - single player *)
         Dream.get "/play_single_player" (fun request ->
             Game.cleanse_board player1_board;
             Board.reset player2_board;
             Cpu.place_cpu_ships player2_board 5;
             Game.cleanse_board player2_board;

             current_turn := "user";
             Dream.html
               (Template.single_player_game_board
                  ~user_board_status:(Board.board_to_string player1_board)
                  ~cpu_board_status:(Board.board_to_string player2_board)
                  ~user_bombs:(Int.to_string !player1_bombs)
                  ~cpu_bombs:(Int.to_string !player2_bombs)
                  ~turn:!current_turn ~game_over:"false" request));
         (* user turn *)
         Dream.post "/user_turn" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("user-move", message) ] ->
                 Core_unix.sleep 1;
                 (* short delay so we can actually see the move being made *)
                 handle_user_turn message request
             | _ -> Dream.empty `Bad_Request);
         (* CPU turn  - it is a GET request because we aren't sending any data *)
         Dream.get "/cpu_turn" (fun request ->
             Core_unix.sleep 1;
             handle_cpu_turn request);
         Dream.get "/reset_board" (fun request ->
             handle_placement_reset request);
         Dream.get "/save" (fun request -> handle_save request);
         Dream.get "/static/**" (Dream.static "./static");
       ]
