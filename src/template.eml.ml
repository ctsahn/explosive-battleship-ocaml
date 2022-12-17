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
      <form action="/load" method="get">
      <input type="submit" 
            value="Load existing game from file"  />
      </form>
      </body>
      </html>

let ship_placement ~turn ~user_board_status ~ship_status ~ready ~error request= 
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

  <body onload=<%s "loadColors('" ^ user_board_status ^ "','" ^ ship_status ^ "')" %>>

%     let display_turn = 
%           if turn = "player1" then "Player 1"
%           else if turn = "player2" then "Player 2" 
%           else "User" in         
  <h1>Place ships - <%s display_turn %></h1>

  <div id="user-board-status" style="display: none;"><%s user_board_status %></div>
  <div id="ship-status" style="display: none;"><%s ship_status %></div>

  <h2 id="length5">Carrier (length of 5)</h2>
  <h2 id="length4">Battleship (length of 4)</h2>
  <h2 id="length3">Cruiser (length of 3) </h2>
  <h2 id="length2">Destroyer (length of 2)</h2>
  <h2 id="length1">Mine (length of 1) </h2>

  <div class="column">
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


%     if (error = Game.not_straight_error) then begin
      <p style="color:red">Invalid placement! Placements must be vertical or horizontal.</p>
%     end
%     else if (error = Game.touching_error) then begin 
      <p style="color:red"> Invalid placement! Ships cannot touch. </p>
%     end
%     else if (error = Game.repeat_error) then begin 
      <p style="color:red"> Repeat placement! There can only be one of each ship size.</p>
%     end
%     else if (error = Game.too_big_error) then begin 
      <p style="color:red"> Ship too large! </p>
%     end;


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

  <form action="/reset_board" method="get">
      
    <button type="submit"> Reset board </button>

  </form>

  </div>


  
  <div style="padding-left: 400px; width:1000px">
  <p> 
  
  Placement instructions:

  <ol>
  <li> Each player must place one type of each ship (Carrier, Battleship, Cruiser, Destroyer, Mine). 
  
  <li>  In order to place one ship, two clicks are required: the first click marks where the ship starts and the second click marks where the ship ends. Ships must be placed either horizontally or vertically.
  
  </li>
  <li>  Ships cannot touch each other; a radius of squares will be formed around a ship when placed. 
  </li>
  <li>  The Mine is a special ship that occupies only one square (it can be placed by clicking the same square twice). When the Mine is hit by your opponent, it automatically hits one of their ships' squares. The turn also becomes yours! 
  </li>

  <li>  The board can be cleared by clicking "Reset board". 
  </li>

  </ol>
  
  </p>
  </div>

  <script src= "static/placement.js">
  </script>
  </body>
  </html>


let two_player_game_board ~player1_board_status ~player2_board_status ~player1_bombs ~player2_bombs ~turn ~game_over request = 
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
        <input id = <%s "player1input" ^ cell_num %> type="hidden" name="player2-move" value=<%s cell_num %> >
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
        <input id=<%s "player2input" ^ cell_num %> type="hidden" name="player1-move" value=<%s cell_num %> >
        </form>
        </td>
%       display_player2_row row (col+1)
%   in  
  <body onload=<%s "loadTwoPlayer('" ^ player1_board_status ^ "','" ^ player2_board_status ^ "'," ^game_over ^")"%> >

%     let turn_display = 
%       if (game_over = "true" && turn = "player1") then "Player 1 wins!"
%       else if (game_over = "true" && turn = "player2") then "Player 2 wins!"                    
%       else if (turn = "player1") then "Player 1's turn!"
%       else "Player 2's turn!" in
  <h1 id="turn"> <%s turn_display %> </h1>

  <div class="column">
  <h2>Player 1's board</h2>
  <p>Player 1's remaining bombs: <%s player1_bombs %></p>
  
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
  </div>
  <div class="column">
  <h2>Player 2's board</h2>
  <p>Player 2's remaining bombs: <%s player2_bombs %></p>
  
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
  </div>

  <form action="/save" method="get">
      

%     if (game_over = "true") then begin
            <button type="submit" disabled > Save game </button>
%     end
%     else begin 
            <button type="submit" onclick="alert('Game saved to saved.txt! (Note that saving again will overwrite this file)')"> Save game </button>
%     end;

  </form>
% if ((turn = "player1" && player1_bombs = "0") ||(turn = "player2" && player2_bombs = "0") || (game_over = "true")) then begin 

      <button disabled>Bomb</button>
% end
% else begin
      <button onclick="bombSet=true; this.disabled=true; this.innerHTML = 'Click to fire bomb!';">Bomb</button>
% end;

      <div style="padding-left:50px;padding-top:50px;width:1500px;">
      <p> 

      Game rules:

      <ol>
      <li> The rules of traditional Battleship apply: a miss results in the opponent's turn and a hit results in your turn continuing. The first player to sink all opponent ships wins!

      <li>  Each player gets 3 Bombs. A Bomb can be fired by clicking "Bomb" and clicking on the square you want to fire at; the Bomb will attack that square and the squares directly surrounding it. 
      </li>
      <li>  When a ship is sunk, all squares around it will be revealed (as ships cannot touch each other).
      </li>
      <li>  A hit Mine is represented by a black square. When the Mine is hit by your opponent, it automatically hits one of their ships' squares. The turn also becomes yours! 
      </li>

      <li>  The game can be saved by clicking "Save game". Only one game can be saved at a time. 
      </li>

      </ol>

      </p>
      </div>


  <script src= "static/two-player.js">
  
  </script>
  </body>
  </html>


let single_player_game_board ~user_board_status ~cpu_board_status ~user_bombs ~cpu_bombs ~turn ~game_over request = 
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
      <input id=<%s "cpuinput" ^ cell_num %> type="hidden" name="user-move" value=<%s cell_num %> >
      </form>
      </td>
%       display_cpu_row row (col+1)
%   in  
<body onload=<%s "loadSinglePlayer('" ^ user_board_status ^ "','" ^ cpu_board_status ^ "'," ^ game_over ^")"%> >

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


<div class="column">
<h2>User's board</h2>
<p>User's remaining bombs: <%s user_bombs %></p>


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
</div>
<div class="column">
<h2>CPU's board</h2>
<p>CPU's remaining bombs: <%s cpu_bombs %></p>
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
</div>
<form action="/save" method="get">
      

%     if (game_over = "true") then begin
            <button type="submit" disabled > Save game </button>
%     end
%     else begin 
            <button type="submit" onclick="alert('Game saved to saved.txt! (Note that saving again will overwrite this file)')"> Save game </button>
%     end;

  </form>
% if ((turn = "user" && user_bombs = "0") ||(turn = "cpu"  )  || (game_over = "true")) then begin 

      <button disabled>Bomb</button>
% end
% else begin
      <button onclick="bombSet=true; this.disabled=true; this.innerHTML = 'Click to fire bomb!';">Bomb</button>
% end;

<div style="padding-left:50px;padding-top:50px;width:1500px;">
<p> 

Game rules:

<ol>
<li> The rules of traditional Battleship apply: a miss results in the opponent's turn and a hit results in your turn continuing. The first player to sink all opponent ships wins!

<li>  Each player gets 3 Bombs. A Bomb can be fired by clicking "Bomb" and clicking on the square you want to fire at; the Bomb will attack that square and the squares directly surrounding it. 
</li>
<li>  When a ship is sunk, all squares around it will be revealed (as ships cannot touch each other).
</li>
<li>  A hit Mine is represented by a black square. When the Mine is hit by your opponent, it automatically hits one of their ships' squares. The turn also becomes yours! 
</li>

<li>  The game can be saved by clicking "Save game". Only one game can be saved at a time. 
</li>

</ol>

</p>
</div>

<script src= "static/single-player.js">

</script>
</body>


</html>

