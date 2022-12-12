open Core

let boards = Board.initialize_boards
let player1_board = fst boards (* User's board when single player *)
let player2_board = snd boards (* CPU's board when single player *)
let current_turn = ref ""
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
    match Game.place_ship player_board !player_ship_status !click1 !click2 with
    (* A valid placement of the ship *)
    | Ok v ->
        ship_size := Int.to_string v;

        player_ship_status := !player_ship_status ^ !ship_size;

        let sorted_ship_sizes =
          List.sort (String.to_list !player_ship_status) ~compare:Char.compare
        in
        let expected_ship_sizes = [ '2'; '3'; '4'; '5' ] in

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
    | Error e -> (* Not valid placement, alert user of error *)
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

let handle_reset request =
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
    (* For converting a tuple into a string used in JSON *)
    let tuple_to_string (tup : int * int) =
      Int.to_string (fst tup) ^ "," ^ Int.to_string (snd tup)
    in

    let save_states =
      {
        user = Board.board_to_string player1_board;
        cpu = Board.board_to_string player2_board;
        turn = !current_turn;
        cpu_horz_queue =
          List.to_string ~f:tuple_to_string (Queue.to_list Game.cpu_horz_queue);
        cpu_vert_queue =
          List.to_string ~f:tuple_to_string (Queue.to_list Game.cpu_vert_queue);
        cpu_direction = !Game.cpu_attack_direction;
      }
    in

    let out_str =
      single_player_save_to_yojson save_states |> Yojson.Safe.to_string
    in
    Out_channel.write_all "saved.txt" ~data:out_str;

    Dream.html
      (Template.single_player_game_board
         ~user_board_status:(Board.board_to_string player1_board)
         ~cpu_board_status:(Board.board_to_string player2_board)
         ~turn:!current_turn ~game_over:"false" request))
  else
    let save_states =
      {
        player1 = Board.board_to_string player1_board;
        player2 = Board.board_to_string player2_board;
        turn = !current_turn;
      }
    in
    let out_str =
      two_player_save_to_yojson save_states |> Yojson.Safe.to_string
    in
    Out_channel.write_all "saved.txt" ~data:out_str;

    Dream.html
      (Template.two_player_game_board
         ~player1_board_status:(Board.board_to_string player1_board)
         ~player2_board_status:(Board.board_to_string player2_board)
         ~turn:!current_turn ~game_over:"false" request)

let handle_load request =
  let load_json = In_channel.read_all "saved.txt" |> Yojson.Safe.from_string in

  (* Load it as a 2-player record first *)
  let record = two_player_save_of_yojson load_json in
  match record with
  | Ok two_player_record ->
      current_turn := two_player_record.turn;
      Board.populate_board player1_board two_player_record.player1;
      Board.populate_board player2_board two_player_record.player2;

      Dream.html
        (Template.two_player_game_board
           ~player1_board_status:(Board.board_to_string player1_board)
           ~player2_board_status:(Board.board_to_string player2_board)
           ~turn:!current_turn ~game_over:"false" request)
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
      Board.populate_board player1_board single_player_record.user;
      Board.populate_board player2_board single_player_record.cpu;
      Game.cpu_attack_direction := single_player_record.cpu_direction;

      (* Populate queues *)
      Queue.clear Game.cpu_horz_queue;
      queue_string_to_list single_player_record.cpu_horz_queue
      |> Queue.enqueue_all Game.cpu_horz_queue;
      Queue.clear Game.cpu_vert_queue;
      queue_string_to_list single_player_record.cpu_vert_queue
      |> Queue.enqueue_all Game.cpu_vert_queue;

      Dream.html
        (Template.single_player_game_board
           ~user_board_status:(Board.board_to_string player1_board)
           ~cpu_board_status:(Board.board_to_string player2_board)
           ~turn:!current_turn ~game_over:"false" request)

(* For 2-player turns *)
let handle_player_turn (user_move : string) (opponent_board : Board.t) request =
  let user_move_int = Int.of_string user_move in

  (* If a ship was hit, true *)
  let ship_hit = Game.attack opponent_board user_move_int in
  if ship_hit then
    if Game.is_game_over opponent_board then
      Dream.html
        (Template.two_player_game_board
           ~player1_board_status:(Board.board_to_string player1_board)
           ~player2_board_status:(Board.board_to_string player2_board)
           ~turn:!current_turn ~game_over:"true" request)
    else
      Dream.html
        (Template.two_player_game_board
           ~player1_board_status:(Board.board_to_string player1_board)
           ~player2_board_status:(Board.board_to_string player2_board)
           ~turn:!current_turn ~game_over:"false" request)
  else (
    (* if miss, rotate turn *)
    current_turn :=
      if String.( = ) !current_turn "player1" then "player2" else "player1";

    Dream.html
      (Template.two_player_game_board
         ~player1_board_status:(Board.board_to_string player1_board)
         ~player2_board_status:(Board.board_to_string player2_board)
         ~turn:!current_turn ~game_over:"false" request))

let cpu_turn request =
  let ship_hit = Game.cpu_attack player1_board in
  if ship_hit then
    if Game.is_game_over player1_board then
      Dream.html
        (Template.single_player_game_board
           ~user_board_status:(Board.board_to_string player1_board)
           ~cpu_board_status:(Board.board_to_string player2_board)
           ~turn:!current_turn ~game_over:"true" request)
    else
      Dream.html
        (Template.single_player_game_board
           ~user_board_status:(Board.board_to_string player1_board)
           ~cpu_board_status:(Board.board_to_string player2_board)
           ~turn:!current_turn ~game_over:"false" request)
  else (
    (* if miss, rotate turn *)
    current_turn := "user";
    Dream.html
      (Template.single_player_game_board
         ~user_board_status:(Board.board_to_string player1_board)
         ~cpu_board_status:(Board.board_to_string player2_board)
         ~turn:!current_turn ~game_over:"false" request))

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
           ~turn:!current_turn ~game_over:"true" request)
    else
      Dream.html
        (Template.single_player_game_board
           ~user_board_status:(Board.board_to_string player1_board)
           ~cpu_board_status:(Board.board_to_string player2_board)
           ~turn:!current_turn ~game_over:"false" request)
  else (
    (* if miss, rotate turn *)
    current_turn := "cpu";
    Dream.html
      (Template.single_player_game_board
         ~user_board_status:(Board.board_to_string player1_board)
         ~cpu_board_status:(Board.board_to_string player2_board)
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
                  ~turn:!current_turn ~game_over:"false" request));
         (* Start out with user turn with a blank message/board status*)
         Dream.post "/player1_turn" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("player1-move", message) ] ->
                 Core_unix.sleep 1;
                 (* short delay so we can actually see the move being made *)
                 handle_player_turn message player2_board request
             | _ -> Dream.empty `Bad_Request);
         Dream.post "/player2_turn" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("player2-move", message) ] ->
                 Core_unix.sleep 1;
                 (* short delay so we can actually see the move being made *)
                 handle_player_turn message player1_board request
             | _ -> Dream.empty `Bad_Request);
         (* play game - single player *)
         Dream.get "/play_single_player" (fun request ->
             Game.cleanse_board player1_board;
             Game.cleanse_board player2_board;
             current_turn := "user";
             Dream.html
               (Template.single_player_game_board
                  ~user_board_status:(Board.board_to_string player1_board)
                  ~cpu_board_status:(Board.board_to_string player2_board)
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
             cpu_turn request);
         Dream.get "/reset_board" (fun request -> handle_reset request);
         Dream.get "/save" (fun request -> handle_save request);
         Dream.get "/static/**" (Dream.static "./static");
       ]
