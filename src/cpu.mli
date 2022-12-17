(** Squares for CPU to attack horizontally *)
val horz_attack_queue : (int * int) Core.Queue.t

(** Squares for CPU to attack vertically *)
val vert_attack_queue : (int * int) Core.Queue.t
(** Current direction the CPU is attacking *)
val attack_direction : string ref

(**
    Given the board to attack, the penalty board (in case of a mine hit), and the number of bombs remaining, the CPU peforms a valid attack.

    The CPU is capable of attacking smartly if a previous hit on a ship exists. 

    Return true if a ship was hit, false otherwise.
*)
val cpu_attack : Board.t -> Board.t -> int ref -> bool

(**
    Randomly place CPU ships on a board, given the size of the largest ship to place
*)
val place_cpu_ships : Board.t -> int -> unit

(**
    Given the board to attack, the penalty board (in case of a mine hit), and row/col of a square, attack the square.
    Return true if a ship was hit, false otherwise. 
*)
val attack_given_coords : Board.t -> Board.t -> int -> int -> bool
