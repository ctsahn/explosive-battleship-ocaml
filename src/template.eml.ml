(* ~message is status of opponent's board *)


let ship_placement ~user_board_status ~ship_status ~placed_ship_size request= 
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
        <td class="light" id=<%s "usercell"^cell_num %>  onclick=<%s "handleShipPlacement('usercell" ^cell_num^ "','userform"^cell_num^"')" %> >
        <form id=<%s "userform" ^ cell_num %> method="post" action="/">
        <%s! Dream.csrf_tag request %>
        <input type="hidden" name="user-place" value=<%s cell_num %> >
        </form>
        </td>
%       display_user_row row (col+1)
%   in 


  <body>

  <h1>Place ships</h1>

  <h2>User ships</h2>
  <div id="user-board-status" style="display: none;"><%s user_board_status %></div>
  <div id="placed-ship-size" style="display: none;"><%s placed_ship_size %></div>
  <div id="ship-status" style="display: none;"><%s ship_status %></div>

  <h2 id="length5">5<h2>
  <h2 id="length4">4<h2>
  <h2 id="length3">3<h2>
  <h2 id="length2">2<h2>
  <h2 id="length1">1<h2>
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

  <form action="/play" method="get">
    <input type="submit" 
         value="Ready"  />
  </form>

  <script src= "static/placement.js">
  
  </script>
  </body>
  </html>


let game_board ~user_board_status ~cpu_board_status ~turn request = 
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
        <td class="light" id=<%s "usercell"^cell_num %>  onclick=<%s "handleClick('usercell" ^cell_num^ "','userform"^cell_num^"')" %> >
        <form id=<%s "userform" ^ cell_num %> method="post" action="/cpu_turn">
        <%s! Dream.csrf_tag request %>
        <input type="hidden" name="cpu-move" value=<%s cell_num %> >
        </form>
        </td>
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
        <td class="light" id=<%s "cpucell"^cell_num %>  onclick=<%s "handleClick('cpucell" ^cell_num^ "','cpuform"^cell_num^"')" %> >
        <form id=<%s "cpuform" ^ cell_num %> method="post" action="/user_turn">
        <%s! Dream.csrf_tag request %>
        <input type="hidden" name="user-move" value=<%s cell_num %> >
        </form>
        </td>
%       display_cpu_row row (col+1)
%   in  
  <body>

  <h1 id="turn"><%s turn %> turn </h1>

  <h2>User ships</h2>
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
  <h2>CPU ships</h2>
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


  <script src= "static/script.js">
  
  </script>
  </body>
  </html>
