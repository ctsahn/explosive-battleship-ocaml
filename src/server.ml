open Core

(* We should probably move the creation of these arrays to game.ml *)

(* 0 for nothing, 1 for miss, 2 for ship, 3 for ship hit, 4 for ship sunk *)
let boards = Board.initialize_boards
let player1_board = fst boards (* User's board when single player *)
let player2_board = snd boards (* CPU's board when single player *)

(* true after two clicks *)
let ship_placed = ref false
let click1 = ref 0
let click2 = ref 0 

let player1_ship_status = ref "" (* User placement of ships - which ship lengths have been placed so far *)
let player2_ship_status = ref "" (* User placement of ships - which ship lengths have been placed so far *)

let ship_size = ref "-1"

let handle_ship_placement (turn: string) (player_board : Board.t) (player_ship_status: string ref) (user_move:string) request  = 
  if !ship_placed then (
    click2 := Int.of_string user_move;
    ship_placed:=false;
    (* let ship_size = Int.to_string (!click2 - !click1+1) in *)
    ship_size := Int.to_string (Game.place_ship player_board !click1 !click2);
    player_ship_status := (!player_ship_status) ^ ship_size.contents;
  )
  else (
    click1 := Int.of_string user_move;
    ship_placed := true;
  );
  
  Dream.html (Template.ship_placement ~turn: turn ~user_board_status: (Board.board_to_string player_board) ~ship_status: (!player_ship_status ) ~placed_ship_size: ship_size.contents request )

(* For 2-player turns *)
let handle_player_turn (user_move:string) (opponent_board:Board.t) (turn: string) request = 

  let user_move_int = Int.of_string user_move in

  (* If a ship was hit, true*)
  let ship_hit = Game.attack opponent_board user_move_int in
  
  (* if hit *)
  if  ship_hit  then (
    
  Dream.html (Template.two_player_game_board  ~player1_board_status:(Board.board_to_string player1_board) ~player2_board_status:(Board.board_to_string player2_board) ~turn:turn request )
  )
  (* if miss, rotate turn *)
  else ( 
    

  let new_turn = 
    if String.(=) turn "player1" then "player2"
    else "player1" in 
  Dream.html (Template.two_player_game_board  ~player1_board_status:(Board.board_to_string player1_board) ~player2_board_status:(Board.board_to_string player2_board) ~turn:new_turn request )
  
  
  )


let cpu_turn request= 
  let ship_hit = Game.cpu_attack player1_board in 
  if ship_hit then 

  Dream.html (Template.single_player_game_board ~user_board_status:(Board.board_to_string player1_board) ~cpu_board_status: (Board.board_to_string player2_board) ~turn:"cpu" request)
else 
  Dream.html (Template.single_player_game_board ~user_board_status:(Board.board_to_string player1_board) ~cpu_board_status: (Board.board_to_string player2_board) ~turn:"user" request)


(* For single player turn - we also handle CPU move here *)
let handle_user_turn (user_move:string) request = 
  (* @julia very simple logic for hit/miss - move this to the attack function i think *)
  let user_move_int = Int.of_string user_move in
  let ship_hit = Game.attack player2_board user_move_int in 

  (* continue user turn*)
  if ship_hit then 
    Dream.html (Template.single_player_game_board ~user_board_status:(Board.board_to_string player1_board) ~cpu_board_status: (Board.board_to_string player2_board) ~turn:"user" request)
else 
  Dream.html (Template.single_player_game_board ~user_board_status:(Board.board_to_string player1_board) ~cpu_board_status: (Board.board_to_string player2_board) ~turn:"cpu" request)


let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [

  (* start screen, choose game type *)
    Dream.get "/"
     (fun _ -> 
      Dream.html (Template.start_screen)
      );

  (* initial ship placement screen - single player *)
    Dream.get "/placement"
     (fun request -> 
      Dream.html (Template.ship_placement ~turn: "user" ~user_board_status: (Board.board_to_string player1_board) ~ship_status: !player1_ship_status ~placed_ship_size: "0" request)
      );
  (* send info of one square for placement *)
    Dream.post "/placement"
    (fun request -> 
      match%lwt Dream.form request with
      | `Ok ["user-place", message] ->
        handle_ship_placement "user" player1_board player1_ship_status message request
      | _ ->
        Dream.empty `Bad_Request);

  (* player1 placement screen - user vs. user *)
  Dream.get "/player1_placement"
  (fun request -> 
   Dream.html (Template.ship_placement ~turn: "player1" ~user_board_status: (Board.board_to_string player1_board) ~ship_status: !player1_ship_status ~placed_ship_size: "0" request)
   );
  (* send info of one square for placement *)
  Dream.post "/player1_placement"
  (fun request -> 
    match%lwt Dream.form request with
    | `Ok ["user-place", message] ->
      handle_ship_placement "player1" player1_board player1_ship_status message request
    | _ ->
      Dream.empty `Bad_Request);

  (* player2 placement screen - user vs. user *)
  Dream.get "/player2_placement"
  (fun request -> 
   Dream.html (Template.ship_placement~turn: "player2" ~user_board_status: (Board.board_to_string player2_board) ~ship_status: !player2_ship_status ~placed_ship_size: "0" request)
   );
  (* send info of one square for placement *)
  Dream.post "/player2_placement"
  (fun request -> 
    match%lwt Dream.form request with
    | `Ok ["user-place", message] ->
      handle_ship_placement "player2" player2_board player2_ship_status message request
    | _ ->
      Dream.empty `Bad_Request);

  (* play game - 2-player *)
    Dream.get  "/play_two_player"
      (fun request ->
        
        Dream.html  (Template.two_player_game_board  ~player1_board_status:(Board.board_to_string player1_board) ~player2_board_status:(Board.board_to_string player2_board) ~turn: "player1" request )); (* Start out with user turn with a blank message/board status*)

    Dream.post "/player1_turn"
    (fun request ->
      match%lwt Dream.form request with
      | `Ok ["player1-move", message] ->
        Core_unix.sleep 1; (* short delay so we can actually see the move being made *)
        handle_player_turn message player2_board "player1" request
        
      | _ ->
        Dream.empty `Bad_Request);
    Dream.post "/player2_turn"
    (fun request ->
      match%lwt Dream.form request with
      | `Ok ["player2-move", message] -> 
        Core_unix.sleep 1; (* short delay so we can actually see the move being made *)
        handle_player_turn message player1_board "player2" request
        
      | _ ->
        Dream.empty `Bad_Request);


    (* play game - single player *)
    Dream.get  "/play_single_player"
      (fun request ->
        
        Dream.html  (Template.single_player_game_board  ~user_board_status:(Board.board_to_string player1_board) ~cpu_board_status:(Board.board_to_string player2_board) ~turn:"user" request )); (* Start out with user turn with a blank message/board status*)

    (* user turn *)
    Dream.post "/user_turn"
    (fun request ->
      match%lwt Dream.form request with
      | `Ok ["user-move", message] ->
        Core_unix.sleep 1; (* short delay so we can actually see the move being made *)
        handle_user_turn message request
        
      | _ ->
        Dream.empty `Bad_Request);

    (* CPU turn  - it is a GET request because we aren't sending any data *)
    Dream.get "/cpu_turn"
    
      (fun request ->
        Core_unix.sleep 1;
        
        (cpu_turn request)
        
      );
    
    

    
    

    Dream.get "/static/**" (Dream.static "./static");

  ]