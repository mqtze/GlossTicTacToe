# Tic Tac Toe in Haskell with Gloss

## Installation & Compilation

### Install Dependencies
```bash
cabal install --lib --package-env . extra parallel lens gloss random
```
### Compile
```bash
ghc Main.hs StrategicGames.lhs -o TicTacToe -package-db [PATH_TO_CABAL_STORE]\ghc-9.6.7\package.db -package gloss -package lens -package parallel -package random -package extra
```

### Start
```bash
./TicTacToe
```

## Game Modes
- **Normal**  
  Classic Tic Tac Toe, the first player to get three in a row wins.

- **Misere**  
  Reversed rules, whoever gets three in a row loses.

- **Notakto**  
  A variation of Misere, but both players use X.

- **Piece-Limit**  
  Only three symbols per player are allowed on the board at a time.