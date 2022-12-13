val horz_attack_queue : (int * int) Core.Queue.t
val vert_attack_queue : (int * int) Core.Queue.t
val attack_direction : string ref

(*
    Given the board to attack and previous hits, the computer chooses a valid position on the 
    opponent's board depending on its previous successful hits. 

    Return true if a ship was hit, false otherwise.
*)
    val cpu_attack : Board.t ->int ref-> bool

(*
    Randomly place CPU ships on a board.
*)
val place_cpu_ships: Board.t -> int -> unit