let start_screen = 
      <html>
      <head>
      <link rel="stylesheet" href="static/style.css">
      </head>

      <body>

      <form action="/placement" method="get">
      <input type="submit" 
            value="New user vs. CPU game"  />
      </form>

      <form action="/player1_placement" method="get">
      <input type="submit" 
            value="New user vs. user game"  />
      </form>
      <form action="/placement" method="get">
      <input type="submit" 
            value="Load existing game from file"  />
      </form>



      </body>
      </html>

let ship_placement ~turn ~user_board_status ~ship_status ~placed_ship_size ~ready request= 
  <html>
  <head>
  <link rel="stylesheet" href="static/style.css">
  </head>
% (* Display each row of the grid *)
%   let rec display_user_row row col = 
%      match col with
%     | 10 -> ()
%     | _ ->
%       let cell_num = 
%         if row = 0 then 
%          (Int.to_string col)
%         else (Int.to_string row)^(Int.to_string col) in 
        <td class="light" id=<%s "usercell"^cell_num %>  onclick=<%s "handleShipPlacement('usercell" ^cell_num^ "','userform"^cell_num^ "'," ^ ready ^")" %> >

%       let placement_endpoint =
%           if turn = "player1" then "/player1_placement"
%           else if turn = "player2" then "/player2_placement" 
%           else "/placement" in 
        <form id=<%s "userform" ^ cell_num %> method="post" action=<%s placement_endpoint %>> 
        <%s! Dream.csrf_tag request %>
        <input type="hidden" name="user-place" value=<%s cell_num %> >
        </form>
        </td>
%       display_user_row row (col+1)
%   in 


  <body>

%     let display_turn = 
%           if turn = "player1" then "Player 1"
%           else if turn = "player2" then "Player 2" 
%           else "User" in         
  <h1>Place ships - <%s display_turn %></h1>

  <div id="user-board-status" style="display: none;"><%s user_board_status %></div>
  <div id="placed-ship-size" style="display: none;"><%s placed_ship_size %></div>
  <div id="ship-status" style="display: none;"><%s ship_status %></div>

  <h2 id="length5">5<h2>
  <h2 id="length4">4<h2>
  <h2 id="length3">3<h2>
  <h2 id="length2">2<h2>
  <table class="board">
      <tbody>
      <tr>
%   begin display_user_row 0 0 end;
      </tr>

      <tr>
%   begin display_user_row 1 0 end;
      </tr

      <tr>
%   begin display_user_row 2 0 end;
      </tr>
      
      <tr>
%   begin display_user_row 3 0 end;
      </tr>

      <tr>
%   begin display_user_row 4 0 end;
      </tr>

      <tr>
%   begin display_user_row 5 0 end;
      </tr>

      <tr>
%   begin display_user_row 6 0 end;
      </tr>

      <tr>
%   begin display_user_row 7 0 end;
      </tr>

      <tr>
%   begin display_user_row 8 0 end;
      </tr>

      <tr>
%   begin display_user_row 9 0 end;
      </tr>

      </tbody>
  </table>


%       let submit_endpoint =
%           if turn = "player1" then "/player2_placement"
%           else if turn = "player2" then "/play_two_player"
%           else "/play_single_player" in 

      <form action=<%s submit_endpoint %> method="get">
%     if (ready = "true") then begin
      <input type="submit" value="Ready!" />
%     end
%     else begin 
      <input type="submit" value="Ready!"  disabled/>
%     end;
      </form>

  <form action="/reset_board" method="post">
      
    <button type="submit" name="reset" value=<%s turn %>> Reset board </button>
  <%s! Dream.csrf_tag request %>
         
  </form>


  <script src= "static/placement.js">
  
  </script>
  </body>
  </html>


let two_player_game_board ~player1_board_status ~player2_board_status ~turn ~game_over request = 
  <html>
  <head>
  <link rel="stylesheet" href="static/style.css">
  </head>

% (* Display each row of the grid *)
%   let rec display_player1_row row col = 
%      match col with
%     | 10 -> ()
%     | _ ->
%       let cell_num = 
%         if row = 0 then 
%          (Int.to_string col)
%         else (Int.to_string row)^(Int.to_string col) in 
        <td class="light" id=<%s "player1cell"^cell_num %>  onclick=<%s "handleClick('player1cell" ^cell_num^ "','player1form"^cell_num^"'" ^ ",'" ^turn^ "'," ^ game_over ^")" %> >
        <form id=<%s "player1form" ^ cell_num %> method="post" action="/player2_turn">
        <%s! Dream.csrf_tag request %>
        <input type="hidden" name="player2-move" value=<%s cell_num %> >
        </form>
        </td>
%       display_player1_row row (col+1)
%   in 

% (* Display each row of the grid *)
%   let rec display_player2_row row col = 
%      match col with
%     | 10 -> ()
%     | _ ->
%       let cell_num = 
%         if row = 0 then 
%          (Int.to_string col)
%         else (Int.to_string row)^(Int.to_string col) in 
        <td class="light" id=<%s "player2cell"^cell_num %>  onclick=<%s "handleClick('player2cell" ^cell_num^ "','player2form"^cell_num^"'" ^ ",'"^turn^"'," ^ game_over ^ ")" %> >
        <form id=<%s "player2form" ^ cell_num %> method="post" action="/player1_turn">
        <%s! Dream.csrf_tag request %>
        <input type="hidden" name="player1-move" value=<%s cell_num %> >
        </form>
        </td>
%       display_player2_row row (col+1)
%   in  
  <body>

%     let turn_display = 
%       if (game_over = "true" && turn = "player1") then "Player 1 wins!"
%       else if (game_over = "true" && turn = "player2") then "Player 2 wins!"                    
%       else if (turn = "player1") then "Player 1's turn!"
%       else "Player 2's turn!" in
  <h1 id="turn"> <%s turn_display %> </h1>

  <h2>Player 1's board</h2>
  <div id="player1-board-status" style="display: none;"><%s player1_board_status %></div>
  <table class="board">
      <tbody>
      <tr>
%   begin display_player1_row 0 0 end;
      </tr>

      <tr>
%   begin display_player1_row 1 0 end;
      </tr

      <tr>
%   begin display_player1_row 2 0 end;
      </tr>
      
      <tr>
%   begin display_player1_row 3 0 end;
      </tr>

      <tr>
%   begin display_player1_row 4 0 end;
      </tr>

      <tr>
%   begin display_player1_row 5 0 end;
      </tr>

      <tr>
%   begin display_player1_row 6 0 end;
      </tr>

      <tr>
%   begin display_player1_row 7 0 end;
      </tr>

      <tr>
%   begin display_player1_row 8 0 end;
      </tr>

      <tr>
%   begin display_player1_row 9 0 end;
      </tr>

      </tbody>
  </table>
  <h2>Player 2's board</h2>
  <div id="player2-board-status" style="display: none;"><%s player2_board_status %></div>
  <table class="board">
  <tbody>
  <tr>
%   begin display_player2_row 0 0 end;
  </tr>

  <tr>
%   begin display_player2_row 1 0 end;
  </tr

  <tr>
%   begin display_player2_row 2 0 end;
  </tr>
  
  <tr>
%   begin display_player2_row 3 0 end;
  </tr>

  <tr>
%   begin display_player2_row 4 0 end;
  </tr>

  <tr>
%   begin display_player2_row 5 0 end;
  </tr>

  <tr>
%   begin display_player2_row 6 0 end;
  </tr>

  <tr>
%   begin display_player2_row 7 0 end;
  </tr>

  <tr>
%   begin display_player2_row 8 0 end;
  </tr>

  <tr>
%   begin display_player2_row 9 0 end;
  </tr>

  </tbody>
  </table>


  <script src= "static/two-player.js">
  
  </script>
  </body>
  </html>


let single_player_game_board ~user_board_status ~cpu_board_status ~turn ~game_over request = 
<html>
<head>
<link rel="stylesheet" href="static/style.css">
</head>

% (* Display each row of the grid *)
%   let rec display_user_row row col = 
%      match col with
%     | 10 -> ()
%     | _ ->
%       let cell_num = 
%         if row = 0 then 
%          (Int.to_string col)
%         else (Int.to_string row)^(Int.to_string col) in 
      <td class="light" id=<%s "usercell"^cell_num %> ></td>
%       display_user_row row (col+1)
%   in 

% (* Display each row of the grid *)
%   let rec display_cpu_row row col = 
%      match col with
%     | 10 -> ()
%     | _ ->
%       let cell_num = 
%         if row = 0 then 
%          (Int.to_string col)
%         else (Int.to_string row)^(Int.to_string col) in 
      <td class="light" id=<%s "cpucell"^cell_num %>  onclick=<%s "handleClick('cpucell" ^cell_num^ "','cpuform"^cell_num^"'," ^game_over^ ")" %> >
      <form id=<%s "cpuform" ^ cell_num %> method="post" action="/user_turn">
      <%s! Dream.csrf_tag request %>
      <input type="hidden" name="user-move" value=<%s cell_num %> >
      </form>
      </td>
%       display_cpu_row row (col+1)
%   in  
<body onload=<%s "handleCPUTurn("^game_over^")" %> >

% if turn = "user" && game_over = "true" then begin
<h1> User wins! </h1>
% end
% else if turn = "cpu" && game_over = "true" then begin 
<h1> CPU wins! </h1>
% end
% else if turn = "cpu" then begin
<form id="cpu-turn" method="get" action="/cpu_turn">
</form>
<h1> CPU turn </h1>
% end
% else begin 
<h1> User turn </h1>
% end; 



<h2>User's board</h2>
<div id="user-board-status" style="display: none;"><%s user_board_status %></div>
<table class="board">
      <tbody>
      <tr>
%   begin display_user_row 0 0 end;
      </tr>

      <tr>
%   begin display_user_row 1 0 end;
      </tr

      <tr>
%   begin display_user_row 2 0 end;
      </tr>
      
      <tr>
%   begin display_user_row 3 0 end;
      </tr>

      <tr>
%   begin display_user_row 4 0 end;
      </tr>

      <tr>
%   begin display_user_row 5 0 end;
      </tr>

      <tr>
%   begin display_user_row 6 0 end;
      </tr>

      <tr>
%   begin display_user_row 7 0 end;
      </tr>

      <tr>
%   begin display_user_row 8 0 end;
      </tr>

      <tr>
%   begin display_user_row 9 0 end;
      </tr>

      </tbody>
</table>
<h2>CPU's board</h2>
<div id="cpu-board-status" style="display: none;"><%s cpu_board_status %></div>
<table class="board">
<tbody>
<tr>
%   begin display_cpu_row 0 0 end;
</tr>

<tr>
%   begin display_cpu_row 1 0 end;
</tr

<tr>
%   begin display_cpu_row 2 0 end;
</tr>

<tr>
%   begin display_cpu_row 3 0 end;
</tr>

<tr>
%   begin display_cpu_row 4 0 end;
</tr>

<tr>
%   begin display_cpu_row 5 0 end;
</tr>

<tr>
%   begin display_cpu_row 6 0 end;
</tr>

<tr>
%   begin display_cpu_row 7 0 end;
</tr>

<tr>
%   begin display_cpu_row 8 0 end;
</tr>

<tr>
%   begin display_cpu_row 9 0 end;
</tr>

</tbody>
</table>


<script src= "static/single-player.js">

</script>
</body>
</html>

