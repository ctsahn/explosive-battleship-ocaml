open Core

(* We should probably move the creation of these arrays to game.ml *)

(* 0 for nothing, 1 for miss, 2 for ship, 3 for ship hit, 4 for ship sunk *)


(* Right now it's a 1D array for simplicity, but we probably want to make it a 2D array later? Not sure *)
let user_board = Array.create ~len:100 0 (* user's board, so seen by CPU during CPU's turn *)
let cpu_board = Array.create ~len:100 0 (* CPU's board, so seen by the user during user's turn *)

(* true after two clicks *)
let ship_placed = ref false
let click1 = ref 0
let click2 = ref 0

let ship_status = ref "" (*User Placement of ships*)

let handle_ship_placement (user_move:string) request  = 
  

  if !ship_placed then (
    click2 := Int.of_string user_move;
    (*Obviously validate clicks - Julia's job to make some kind of function in the game logic *)
    (* Don't change this status if second click is not valid? *)
    (* VALIDATE HERE @julia *)
    Array.fill user_board ~pos:!click1 ~len:(!click2 - !click1+1) 2; (* This only works for left-to-right horizontal clicks lol*)
    ship_placed:=false;
    let ship_size = Int.to_string (!click2 - !click1+1) in
    ship_status:= (!ship_status) ^ ship_size;)
  else (
      click1 := Int.of_string user_move;
      ship_placed := true;
      );
  
  
  Dream.html (Template.ship_placement ~user_board_status: (Game.board_to_string user_board) ~ship_status: (!ship_status ) ~placed_ship_size: (Int.to_string (!click2 - !click1+1)) request )
(* Handle user input *)
let handle_user_turn (user_move:string) request = 

  (* @julia very simple logic for hit/miss - move this to the attack function i think *)
  let user_move_int = Int.of_string user_move in 
  if  cpu_board.(user_move_int) = 2 then (
  cpu_board.(user_move_int) <- 3;)
  else ( 
  cpu_board.(user_move_int) <- 1;); 

  Dream.html (Template.game_board  ~user_board_status:(Game.board_to_string user_board) ~cpu_board_status:(Game.board_to_string cpu_board) ~turn:"cpu" request ) (* Load CPU turn after processing user move *)

let handle_cpu_turn (cpu_move:string) request = 

  (* @julia very simple logic for hit/miss - move this to the attack function i think *)
  let cpu_move_int = Int.of_string cpu_move in 
  if  user_board.(cpu_move_int) = 2 then (
    user_board.(cpu_move_int) <- 3;)
  else ( 
    user_board.(cpu_move_int) <- 1;); 
  
  Dream.html (Template.game_board  ~user_board_status:(Board.board_to_string user_board) ~cpu_board_status:(Board.board_to_string cpu_board) ~turn: "user" request ) (* Load CPU turn after processing user move *)


let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [

    Dream.get "/"
     (fun request -> 
      Dream.html (Template.ship_placement ~user_board_status: (Game.board_to_string user_board) ~ship_status: !ship_status ~placed_ship_size: "0" request)
      );
    (* send info of one square for placement *)
    Dream.post "/"
    (fun request -> 
      match%lwt Dream.form request with
      | `Ok ["user-place", message] ->
        handle_ship_placement message request
      | _ ->
        Dream.empty `Bad_Request);
      
    Dream.get  "/play"
      (fun request ->
        (* just some dummy placements for testing *)
        cpu_board.(12) <- 2;
        cpu_board.(13) <- 2;
        Dream.html  (Template.game_board  ~user_board_status:(Game.board_to_string user_board) ~cpu_board_status:(Game.board_to_string cpu_board) ~turn: "user" request )); (* Start out with user turn with a blank message/board status*)

    Dream.post "/user_turn"
    (fun request ->
      match%lwt Dream.form request with
      | `Ok ["user-move", message] ->
        Core_unix.sleep 1; (* short delay so we can actually see the move being made *)
        handle_user_turn message request
        
      | _ ->
        Dream.empty `Bad_Request);
    Dream.post "/cpu_turn"
    (fun request ->
      match%lwt Dream.form request with
      | `Ok ["cpu-move", message] -> 
        Core_unix.sleep 1; (* short delay so we can actually see the move being made *)
        handle_cpu_turn message request
        
      | _ ->
        Dream.empty `Bad_Request);

    Dream.get "/static/**" (Dream.static "./static");

  ]