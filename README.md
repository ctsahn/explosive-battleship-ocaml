# Explosive Battleship

To run:

1. Run `dune build` in the root directory of the repo.
2. Run `opam install .` to download dependencies.
3. Run `dune build` again.
4. Run `dune exec src/server.exe` and go to http://localhost:8080 to play.

## Ship placement instructions

1. Each player must place one type of each ship (Carrier, Battleship, Cruiser, Destroyer, Mine). 
  
2. In order to place one ship, two clicks are required: the first click marks where the ship starts and the second click marks where the ship ends. Ships must be placed either horizontally or vertically.
  
3. Ships cannot touch each other; a radius of squares will be formed around a ship when placed. 
4. The Mine is a special ship that occupies only one square (it can be placed by clicking the same square twice). When the Mine is hit by your opponent, it automatically hits one of their ships' squares. The turn also becomes yours! 
5. The board can be cleared by clicking "Reset board". 
  
## Game rules


1. The rules of traditional Battleship apply: a miss results in the opponent's turn and a hit results in your turn continuing. The first player to sink all opponent ships wins!

2. Each player gets 3 Bombs. A Bomb can be fired by clicking "Bomb" and clicking on the square you want to fire at; the Bomb will attack that square and the squares directly surrounding it. 
3. When a ship is sunk, all squares around it will be revealed (as ships cannot touch each other).
4. A hit Mine is represented by a black square. When the Mine is hit by your opponent, it automatically hits one of their ships' squares. The turn also becomes yours! 
5. The game can be saved by clicking "Save game". Only one game can be saved at a time. 

### Presentation slides: https://docs.google.com/presentation/d/1PQkCP9QiDEDx8Atvp3nCCStck-1bo1hXNrRsrbXvL8U/edit?usp=sharing 
