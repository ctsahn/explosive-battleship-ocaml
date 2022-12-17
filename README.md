# Explosive Battleship

To run:

1. Run `dune build` in the root directory of the repo.
2. Run `opam install .` to download dependencies.
3. Run `dune build` again.
4. Run `dune exec src/server.exe` and go to http://localhost:8080 to play.

## Game rules


1. The rules of traditional Battleship apply: a miss results in the opponent's turn and a hit results in your turn continuing. The first player to sink all opponent ships wins!

2. Each player gets 3 Bombs. A Bomb can be fired by clicking "Bomb" and clicking on the square you want to fire at; the Bomb will attack that square and the squares directly surrounding it. 

<li>  When a ship is sunk, all squares around it will be revealed (as ships cannot touch each other).
</li>
<li>  A hit Mine is represented by a black square. When the Mine is hit by your opponent, it automatically hits one of their ships' squares. The turn also becomes yours! 
</li>

<li>  The game can be saved by clicking "Save game". Only one game can be saved at a time. 
</li>

</ol>

</p>
