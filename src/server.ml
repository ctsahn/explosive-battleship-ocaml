open Core

(* We should probably move the creation of these arrays to game.ml *)

(* 0 for nothing, 1 for miss, 2 for ship, 3 for hit? *)


(* Right now it's a 1D array for simplicity, but we probably want to make it a 2D array later? Not sure *)
let user_board = Array.create ~len:100 0 (* user's board, so seen by CPU during CPU's turn *)
let cpu_board = Array.create ~len:100 0 (* CPU's board, so seen by the user during user's turn *)

(* Handle user input *)
let handle_user_turn (user_move:string) request = 

  cpu_board.(Int.of_string user_move) <- 1; (* Update CPU board with the user move (hit/miss - right now everything is a miss)*)
  
  Dream.html (Template.cpu_turn  ~message:(Game.board_to_string user_board) request ) (* Load CPU turn after processing user move *)

let handle_cpu_turn (cpu_move:string) request = 

  user_board.(Int.of_string cpu_move) <- 1; (* Update user board with the CPU move *)
  
  Dream.html (Template.user_turn  ~message:(Game.board_to_string cpu_board) request ) (* Load user turn after processing CPU move *)


let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [

    Dream.get  "/"
      (fun request ->
        Dream.html (Template.user_turn ~message:"" request )); (* Start out with user turn with a blank message/board status*)

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