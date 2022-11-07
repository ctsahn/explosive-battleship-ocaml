open Core
(* This is where the logic of the game goes *)
let board_to_string board =
  (String.concat ~sep:"" (List.map ~f:string_of_int ((Array.to_list board ))))