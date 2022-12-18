(lang dune 2.0)
(name Battleship) 

(generate_opam_files true)
(authors "Curtis Ahn" "Julia Bian")
(maintainers "sahn33@jh.edu" "jbian6@jhu.edu")

(package
 (name Battleship) 
 (synopsis "An OCaml library for Playing Battleship")
 (description "Users can play battleship on a Dream web server, either playing against the computer or another player.")
 (depends
  (ounit2 (>= 2.2.6)) 
  (core (>= 0.15.0))
  (core_unix (>= 0.15.0)) 
  (dream (= 1.0.0~alpha4))
  (lwt (>= 5.6.1))
  (lwt_ppx (>= 2.1.0))
  (yojson (>= 2.0.2))
  (bisect_ppx (>= 2.8.0))
  (ppx_deriving_yojson (>= 3.7.0))
  )
)