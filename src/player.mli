(**
Given a board to attack, penalty board (in case of a mine hit), row/col of square given by human player, and whether game is single player, perform a standard attack

Return true if ship hit, false if otherwise. 
*)
val player_attack : Board.t -> Board.t -> int -> int -> bool -> bool

(**
Given a board to attack, penalty board (in case of a mine hit), row/col of square given by human player, and whether game is single player, perform a bomb attack.

Return true if a ship hit, false if otherwise. 
*)
val player_bomb : Board.t -> Board.t -> int -> int -> bool -> bool
