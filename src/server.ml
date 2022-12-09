open Core

let boards = Board.initialize_boards
let player1_board = fst boards (* User's board when single player *)
let player2_board = snd boards (* CPU's board when single player *)
let ship_placed = ref false (* true after two clicks *)
let click1 = ref 0
let click2 = ref 0
let player1_ship_status = ref ""
(* User placement of ships - which ship lengths have been placed so far *)

let player2_ship_status = ref ""
(* User placement of ships - which ship lengths have been placed so far *)

let handle_ship_placement (turn : string) (player_board : Board.t)
    (player_ship_status : string ref) (user_move : string) request =
  if !ship_placed then (
    click2 := Int.of_string user_move;
    let _ = Game.place_ship player_board !click1 !click2 in
    ship_placed := false;
    let ship_size = Int.to_string (!click2 - !click1 + 1) in
    player_ship_status := !player_ship_status ^ ship_size)
  else (
    click1 := Int.of_string user_move;
    ship_placed := true);

  Dream.html
    (Template.ship_placement ~turn
       ~user_board_status:(Board.board_to_string player_board)
       ~ship_status:!player_ship_status
       ~placed_ship_size:(Int.to_string (!click2 - !click1 + 1))
       request)

let handle_reset (turn : string) request =
  ship_placed := false;
  click1 := 0;
  click2 := 0;
  if String.( = ) turn "player1" || String.( = ) turn "user" then (
    Board.reset player1_board;
    player1_ship_status := "";
    Dream.html
      (Template.ship_placement ~turn
         ~user_board_status:(Board.board_to_string player1_board)
         ~ship_status:!player1_ship_status ~placed_ship_size:"0" request))
  else (
    Board.reset player2_board;
    player2_ship_status := "";
    Dream.html
      (Template.ship_placement ~turn
         ~user_board_status:(Board.board_to_string player2_board)
         ~ship_status:!player2_ship_status ~placed_ship_size:"0" request))

(* For 2-player turns *)
let handle_player_turn (user_move : string) (opponent_board : Board.t)
    (turn : string) request =
  let user_move_int = Int.of_string user_move in

  (* If a ship was hit, true *)
  let ship_hit = Game.attack opponent_board user_move_int in

  if ship_hit then
    if Game.is_game_over opponent_board then
      Dream.html
        (Template.two_player_game_board
           ~player1_board_status:(Board.board_to_string player1_board)
           ~player2_board_status:(Board.board_to_string player2_board)
           ~turn ~game_over:"true" request)
    else
      Dream.html
        (Template.two_player_game_board
           ~player1_board_status:(Board.board_to_string player1_board)
           ~player2_board_status:(Board.board_to_string player2_board)
           ~turn ~game_over:"false" request)
  else
    (* if miss, rotate turn *)
    let new_turn =
      if String.( = ) turn "player1" then "player2" else "player1"
    in
    Dream.html
      (Template.two_player_game_board
         ~player1_board_status:(Board.board_to_string player1_board)
         ~player2_board_status:(Board.board_to_string player2_board)
         ~turn:new_turn ~game_over:"false" request)

let cpu_turn request =
  let ship_hit = Game.cpu_attack player1_board in
  if ship_hit then
    if Game.is_game_over player1_board then
      Dream.html
        (Template.single_player_game_board
           ~user_board_status:(Board.board_to_string player1_board)
           ~cpu_board_status:(Board.board_to_string player2_board)
           ~turn:"cpu" ~game_over:"true" request)
    else
      Dream.html
        (Template.single_player_game_board
           ~user_board_status:(Board.board_to_string player1_board)
           ~cpu_board_status:(Board.board_to_string player2_board)
           ~turn:"cpu" ~game_over:"false" request)
  else
    (* if miss, rotate turn *)
    Dream.html
      (Template.single_player_game_board
         ~user_board_status:(Board.board_to_string player1_board)
         ~cpu_board_status:(Board.board_to_string player2_board)
         ~turn:"user" ~game_over:"false" request)

(* For single player turn *)
let handle_user_turn (user_move : string) request =
  let user_move_int = Int.of_string user_move in
  let ship_hit = Game.attack player2_board user_move_int in
  if ship_hit then
    if Game.is_game_over player2_board then
      Dream.html
        (Template.single_player_game_board
           ~user_board_status:(Board.board_to_string player1_board)
           ~cpu_board_status:(Board.board_to_string player2_board)
           ~turn:"user" ~game_over:"true" request)
    else
      Dream.html
        (Template.single_player_game_board
           ~user_board_status:(Board.board_to_string player1_board)
           ~cpu_board_status:(Board.board_to_string player2_board)
           ~turn:"user" ~game_over:"false" request)
  else
    (* if miss, rotate turn *)
    Dream.html
      (Template.single_player_game_board
         ~user_board_status:(Board.board_to_string player1_board)
         ~cpu_board_status:(Board.board_to_string player2_board)
         ~turn:"cpu" ~game_over:"false" request)

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.router
       [
         (* start screen, choose game type *)
         Dream.get "/" (fun _ -> Dream.html Template.start_screen);
         (* initial ship placement screen - single player *)
         Dream.get "/placement" (fun request ->
             Dream.html
               (Template.ship_placement ~turn:"user"
                  ~user_board_status:(Board.board_to_string player1_board)
                  ~ship_status:!player1_ship_status ~placed_ship_size:"0"
                  request));
         (* send position of one click for ship placement *)
         Dream.post "/placement" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("user-place", message) ] ->
                 handle_ship_placement "user" player1_board player1_ship_status
                   message request
             | _ -> Dream.empty `Bad_Request);
         (* player1 placement screen - user vs. user *)
         Dream.get "/player1_placement" (fun request ->
             Dream.html
               (Template.ship_placement ~turn:"player1"
                  ~user_board_status:(Board.board_to_string player1_board)
                  ~ship_status:!player1_ship_status ~placed_ship_size:"0"
                  request));
         (* send info of one square for placement *)
         Dream.post "/player1_placement" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("user-place", message) ] ->
                 handle_ship_placement "player1" player1_board
                   player1_ship_status message request
             | _ -> Dream.empty `Bad_Request);
         (* player2 placement screen - user vs. user *)
         Dream.get "/player2_placement" (fun request ->
             Dream.html
               (Template.ship_placement ~turn:"player2"
                  ~user_board_status:(Board.board_to_string player2_board)
                  ~ship_status:!player2_ship_status ~placed_ship_size:"0"
                  request));
         (* send info of one square for placement *)
         Dream.post "/player2_placement" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("user-place", message) ] ->
                 handle_ship_placement "player2" player2_board
                   player2_ship_status message request
             | _ -> Dream.empty `Bad_Request);
         (* play game - 2-player *)
         Dream.get "/play_two_player" (fun request ->
             Dream.html
               (Template.two_player_game_board
                  ~player1_board_status:(Board.board_to_string player1_board)
                  ~player2_board_status:(Board.board_to_string player2_board)
                  ~turn:"player1" ~game_over:"false" request));
         (* Start out with user turn with a blank message/board status*)
         Dream.post "/player1_turn" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("player1-move", message) ] ->
                 Core_unix.sleep 1;
                 (* short delay so we can actually see the move being made *)
                 handle_player_turn message player2_board "player1" request
             | _ -> Dream.empty `Bad_Request);
         Dream.post "/player2_turn" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("player2-move", message) ] ->
                 Core_unix.sleep 1;
                 (* short delay so we can actually see the move being made *)
                 handle_player_turn message player1_board "player2" request
             | _ -> Dream.empty `Bad_Request);
         (* play game - single player *)
         Dream.get "/play_single_player" (fun request ->
             Dream.html
               (Template.single_player_game_board
                  ~user_board_status:(Board.board_to_string player1_board)
                  ~cpu_board_status:(Board.board_to_string player2_board)
                  ~turn:"user" ~game_over:"false" request));
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
             cpu_turn request);
         Dream.post "/reset_board" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("reset", message) ] -> handle_reset message request
             | _ -> Dream.empty `Bad_Request);
         Dream.get "/static/**" (Dream.static "./static");
       ]
