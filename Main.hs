{-# LANGUAGE FlexibleInstances #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (find)

import System.Random (StdGen, newStdGen, randomR, mkStdGen)

import StrategicGames

data GuiState = Menu MenuState
              | Game GameState
              | Credits CreditsState
              | Rules RulesState

data MenuState = MenuState
  { selectedOption :: Int -- 0 for Normal, 1 for Misere, 2 for Notakto, 3 for Piece-Limit
  , randomStartEnabled :: Bool
  , rng :: StdGen
  , twoPlayerMode :: Bool
  , startPlayer :: Player
  }

data GameState = GameState
  { gameState :: TicTacToe P
  , cellSize :: Int
  , randomStartEnabledFromMenu :: Bool
  , gameRng :: StdGen
  , twoPlayerModeFromMenu :: Bool
  , startPlayerFromMenu :: Player
  }

data CreditSymbol= CreditSymbol
  { symbolPos :: (Float, Float)
  , symbolVel :: (Float, Float)
  , symbolChar :: Char
  }

data CreditsState = CreditsState
  { creditsSymbols :: [CreditSymbol]
  , creditsRng :: StdGen
  }

data RulesState = RulesState
  { rulesRng :: StdGen --preserve value
  }

initialCreditsState :: StdGen -> CreditsState
initialCreditsState initialRng =
  let
    (symbol, newRng) = generateInitialSymbols 40 initialRng
  in
    CreditsState symbol newRng
  where
    generateInitialSymbols :: Int -> StdGen -> ([CreditSymbol], StdGen)
    generateInitialSymbols 0 rng = ([], rng)
    generateInitialSymbols n rng =
      let
        (char1, rng1) = randomR (0,1) rng :: (Int, StdGen)
        char = if char1 == 0 then 'X' else '0'
        maxPos = 310
        maxSpeed = 20
        (x,rng2) = randomR (-maxPos, maxPos) rng1
        (y,rng3) = randomR (-maxPos, maxPos) rng2
        (vx, rng4) = randomR (-maxSpeed, maxSpeed) rng3
        (vy, rng5) = randomR (-maxSpeed, maxSpeed) rng4
        (finalVx,finalVy) =
          if vx == 0 && vy == 0
            then (maxSpeed/2, -3)
            else (vx,vy)

        symbol = CreditSymbol (x,y) (finalVx,finalVy) char
        (restSymbols, finalRng) = generateInitialSymbols (n-1) rng5
      in
        (symbol : restSymbols, finalRng)



winningLine :: TicTacToe P -> Maybe [(Int, Int)]
winningLine (TTT _ board _ _ _ _) = find isWin coords
  where
    coords = [ [(0,0),(0,1),(0,2)]
             , [(1,0),(1,1),(1,2)]
             , [(2,0),(2,1),(2,2)]
             , [(0,0),(1,0),(2,0)]
             , [(0,1),(1,1),(2,1)]
             , [(0,2),(1,2),(2,2)]
             , [(0,0),(1,1),(2,2)]
             , [(0,2),(1,1),(2,0)] ]
    isWin [(x1,y1),(x2,y2),(x3,y3)] =
      let i (x,y) = board !! (3*x + y)
      in i (x1,y1) /= None && i (x1,y1) == i (x2,y2) && i (x2,y2) == i (x3,y3)

renderMenu :: MenuState -> Picture
renderMenu (MenuState selOpt randEnabled rngVal twoPlayerModeVal startPlayerVal) =
  let
    menuOffset = 200
    randomColor = if randEnabled then green else white
    twoPlayerColor = if twoPlayerModeVal then green else white
    lineHeight = 40
    rl = -250
    (startPlayerText,startPlayerColor) = case startPlayerVal of
                                          One -> ("X", red)
                                          Two -> ("0", blue)
                                          None-> ("Random", orange)
  in
    pictures
      [ translate rl      menuOffset $ scale 0.3 0.3 $ color white $ text "Tic Tac Toe"
      , translate rl      (menuOffset -     lineHeight) $ scale 0.2 0.2 $ color (if selOpt == 0 then green else white) $ text "Normal Mode"
      , translate rl      (menuOffset - 2 * lineHeight) $ scale 0.2 0.2 $ color (if selOpt == 1 then green else white) $ text "Misere Mode"
      , translate (rl+20) (menuOffset - 3 * lineHeight) $ scale 0.2 0.2 $ color (if selOpt == 2 then green else white) $ text "Notakto"
      , translate rl      (menuOffset - 4 * lineHeight) $ scale 0.2 0.2 $ color (if selOpt == 3 then green else white) $ text "Piece Limit"
      , translate rl      (menuOffset - 5 * lineHeight) $ scale 0.15 0.15 $ color randomColor $ text "Random Start (Spacebar an/aus)"
      , translate rl      (menuOffset - 6 * lineHeight) $ scale 0.15 0.15 $ color twoPlayerColor $ text "Two-Player Mode (P an/aus)"
      , translate rl      (menuOffset - 7 * lineHeight) $ scale 0.15 0.15 $ color startPlayerColor $ text ("Starting Player: " ++ startPlayerText ++ " (S to toggle)")
      , translate rl      (menuOffset - 8 * lineHeight) $ scale 0.15 0.15 $ color white $ text "Arrow keys up/down to select,"
      , translate rl      (menuOffset - 9 * lineHeight) $ scale 0.15 0.15 $ color white $ text "Enter to Play"
      , translate rl      (menuOffset - 10.5 * lineHeight) $ scale 0.2 0.2 $ color white $ text "Rules (R)"
      , translate rl      (menuOffset - 11.5 * lineHeight) $ scale 0.2 0.2 $ color white $ text "Credits (C)"
      ]

renderTTT :: GameState -> Picture
renderTTT (GameState ttt@(TTT currentPlayer board currentMode _ _ _) w randEnabledFromMenu _ twoPlayerModeFromMenuVal _) =
  translate (-1.5*w') (-1.5*w') $
    pictures $ drawWinBackground ++ drawGrid ++ drawMoves ++ drawOverlay ++ drawModeDisplay ++ drawKeys
  where
    w' = fromIntegral w

    drawGrid =
      [ color white $ line [ (x*w', 0), (x*w', 3*w') ] | x <- [0..3] ] ++
      [ color white $ line [ (0, y*w'), (3*w', y*w') ] | y <- [0..3] ]

    drawMoves =
      [ translate (fromIntegral y * w' + w'/2 - 23) (fromIntegral (2 - x) * w' + w'/2 - 30)
          $ scale 0.6 0.6
          $ color (if p == One then red else blue)
          $ text (show p)
      | (i, p) <- zip [0..] board
      , p /= None
      , let x = i `div` 3
      , let y = i `mod` 3
      ]

    drawWinBackground = case winningLine ttt of
      Just coords ->
        let winner = board !! (let (x, y) = head coords in 3 * x + y)
            bgColor = if winner == One
                      then makeColor 1 0 0 0.5 --red
                      else makeColor 0 0 1 0.5 --blue
        in [ translate (fromIntegral y * w' + w'/2) (fromIntegral (2 - x) * w' + w'/2)
               $ color bgColor $ rectangleSolid w' w'
           | (x, y) <- coords
           ]
      _ -> []

    drawOverlay
      | hasWinner ttt =
        let (winnerText2,winnerColor2) = case currentPlayer of
                        One -> case currentMode of
                            Normal -> ("0 wins!",blue)
                            Misere -> ("X wins!",red)
                            Notakto ->  ("Player 1 wins!",green)
                            PieceLimit -> ("0 wins!",blue)
                        Two -> case currentMode of
                            Normal -> ("X wins!",red)
                            Misere -> ("0 wins!",blue)
                            Notakto ->  ("Player 2 wins!",green)
                            PieceLimit -> ("X wins!",red)

            winnerText = (if currentPlayer == One then "0 wins!" else "X wins!")
            winnerColor = (if currentPlayer == One then blue else red)
        in
          [translate 0 (3.2*w') $ scale 0.2 0.2 $ color winnerColor2 $ text winnerText2]
      | gameOver ttt =
          [translate 0 (3.2*w') $ scale 0.2 0.2 $ color yellow $ text "It's a tie!"]
      | otherwise =
                let (currentText,currentColor) = case currentPlayer of
                        One -> case currentMode of
                            Normal -> ("X",red)
                            Misere -> ("X",red)
                            Notakto ->  ("Player 1",green)
                            PieceLimit -> ("X",red)
                        Two -> case currentMode of
                            Normal -> ("0",blue)
                            Misere -> ("0",blue)
                            Notakto ->  ("Player 2",green)
                            PieceLimit -> ("0",blue)
                in
          [translate 0 (3.2*w') $ scale 0.2 0.2 $ color currentColor $ text (currentText ++ " to move")]

    drawModeDisplay = [translate (2.3*w') (3.2*w') $ scale 0.2 0.2 $ color white $ text ("(" ++ show currentMode ++ ")")]

    drawKeys = [translate 0 (-(0.3*w')) $ scale 0.2 0.2 $ color white $ text ("Menu (M)  " ++ if gameOver ttt then "Restart (R)" else "")]


renderCredits :: CreditsState -> Picture
renderCredits (CreditsState symbols _) =
  let
    menuOffset = 200
    lineHeight = 40
    rl = -250
  in
    pictures
      ([ color white $ translate rl      menuOffset                   $ scale 0.3 0.3 $ text "Tic Tac Toe"
      , color white $ translate rl      (menuOffset - 2 * lineHeight) $ scale 0.25 0.25 $ text "Developed by:"
      , color white $ translate (rl+20) (menuOffset - 3 * lineHeight) $ scale 0.2 0.2 $ color red $ text "mqtze"
      , color white $ translate (rl+20) (menuOffset - 4 * lineHeight) $ scale 0.2 0.2 $ color blue $ text "-----------"
      , color white $ translate rl      (menuOffset - 6 * lineHeight) $ scale 0.25 0.25 $ text "Technologies Used:"
      , color white $ translate (rl+20) (menuOffset - 7 * lineHeight) $ scale 0.2 0.2 $ text "Haskell"
      , color white $ translate (rl+20) (menuOffset - 8 * lineHeight) $ scale 0.2 0.2 $ text "Gloss"
      , color white $ translate rl      (menuOffset - 11.5 * lineHeight) $ scale 0.2 0.2 $ text "Menu (M)"
      ] ++
      [ translate x y $ scale 0.1 0.1 $ color (if c == 'X' then red else blue) $ text [c]
      | (CreditSymbol (x,y) _ c) <- symbols
      ])

renderRules :: RulesState -> Picture
renderRules (RulesState _) =
  let
    textOffset = 200
    lineHeight = 40
    rl = -250
    unterpunkt = 20
  in
    pictures
      [ translate rl textOffset $ scale 0.3 0.3 $ color white $ text "Game Rules:"
      , translate rl              (textOffset -     lineHeight) $ scale 0.2 0.2   $ color white $ text "Normal Mode:"
      , translate (rl+unterpunkt) (textOffset - 2 * lineHeight) $ scale 0.15 0.15 $ color white $ text "The goal is to get three in a row."
      , translate rl              (textOffset - 3 * lineHeight) $ scale 0.2 0.2   $ color white $ text "Misere Mode:"
      , translate (rl+unterpunkt) (textOffset - 4 * lineHeight) $ scale 0.15 0.15 $ color white $ text "Whoever gets three in a row loses!"
      , translate rl              (textOffset - 5 * lineHeight) $ scale 0.2 0.2   $ color white $ text "Notakto:"
      , translate (rl+unterpunkt) (textOffset - 6 * lineHeight) $ scale 0.15 0.15 $ color white $ text "Misere, but both players use 'X'."
      , translate rl              (textOffset - 7 * lineHeight) $ scale 0.2 0.2   $ color white $ text "Piece Limit:"
      , translate (rl+unterpunkt) (textOffset - 8 * lineHeight) $ scale 0.15 0.15 $ color white $ text "Max. 3 symbols per player at a time."

      , translate rl              (textOffset - 11 * lineHeight) $ scale 0.2 0.2  $ color white $ text "To Menu (M)"
      ]


initialGameFromMenu :: GameMode -> Bool -> StdGen -> Bool -> Player -> (TicTacToe P, StdGen)
initialGameFromMenu mode randomEnabled currentRng twoPlayerModeFlag startPlayer =
  let (newStartPlayer, newRng) = case startPlayer of
                          One -> (One, currentRng)
                          Two -> (Two, currentRng)
                          None ->
                            let (randVal, nextRng) = randomR (0,1) currentRng  :: (Int, StdGen)
                            in (if randVal == 0 then One else Two, nextRng)

  in
  if randomEnabled
  then
    let
      (pos1, r1) = randomR (0, 8) newRng
      (pos2_raw, r2) = randomR (0, 7) r1
      pos2 = if pos2_raw >= pos1 then pos2_raw + 1 else pos2_raw

      emptyBoard = [None, None, None, None, None, None, None, None, None]
      boardWithOne = take pos1 emptyBoard ++ [One] ++ drop (pos1 + 1) emptyBoard
      finalBoard = take pos2 boardWithOne ++ [Two] ++ drop (pos2 + 1) boardWithOne

      initialPlacedPieces = [P (div pos1 3, mod pos1 3), P (div pos2 3, mod pos2 3)]
    in
      (TTT newStartPlayer finalBoard mode True initialPlacedPieces twoPlayerModeFlag, r2)
  else
    (TTT newStartPlayer [None, None, None, None, None, None, None, None, None] mode False [] twoPlayerModeFlag, currentRng)


handleEvent :: Event -> GuiState -> GuiState
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) (Menu (MenuState selOpt randEnabled rngVal twoPlayer startPlayer)) =
    Menu (MenuState (max 0 (selOpt - 1)) randEnabled rngVal twoPlayer startPlayer)
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) (Menu (MenuState selOpt randEnabled rngVal twoPlayer startPlayer)) =
    Menu (MenuState (min 3 (selOpt + 1)) randEnabled rngVal twoPlayer startPlayer)

handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Menu (MenuState selOpt randEnabled currentRng twoPlayerModeFlag startPlayer)) =
    let gameModeSelected = case selOpt of
                            0 -> Normal
                            1 -> Misere
                            2 -> Notakto
                            3 -> PieceLimit
        (newGame, newRng) = initialGameFromMenu gameModeSelected randEnabled currentRng twoPlayerModeFlag startPlayer
    in Game (GameState newGame 150 randEnabled newRng twoPlayerModeFlag startPlayer)

handleEvent (EventKey (SpecialKey KeySpace) Down _ _) (Menu (MenuState selOpt randEnabled rngVal twoPlayerEnabled startPlayer)) =
    Menu (MenuState selOpt (not randEnabled) rngVal twoPlayerEnabled startPlayer)

handleEvent (EventKey (Char 'p') Down _ _) (Menu (MenuState selOpt randEnabled rngVal twoPlayerEnabledVal startPlayer)) =
    Menu (MenuState selOpt randEnabled rngVal (not twoPlayerEnabledVal) startPlayer)
handleEvent (EventKey (Char 'P') Down _ _) (Menu (MenuState selOpt randEnabled rngVal twoPlayerEnabledVal startPlayer)) =
    Menu (MenuState selOpt randEnabled rngVal (not twoPlayerEnabledVal) startPlayer)

handleEvent (EventKey (Char 's') Down _ _) (Menu (MenuState selOpt randEnabled rngVal twoPlayerEnabledVal startPlayer)) =
  let newStartPlayer = case startPlayer of
                          One -> Two
                          Two -> None
                          None -> One
  in
    Menu (MenuState selOpt randEnabled rngVal twoPlayerEnabledVal newStartPlayer)
handleEvent (EventKey (Char 'S') Down _ _) (Menu (MenuState selOpt randEnabled rngVal twoPlayerEnabledVal startPlayer)) =
  let newStartPlayer = case startPlayer of
                          One -> Two
                          Two -> None
                          None -> One
  in
    Menu (MenuState selOpt randEnabled rngVal twoPlayerEnabledVal newStartPlayer)

handleEvent (EventKey (Char 'm') Down _ _) (Game (GameState _ _ _ gameRngVal twoPlayerModeFromMenu startPlayer)) =
    Menu (MenuState 0 False gameRngVal twoPlayerModeFromMenu startPlayer)
handleEvent (EventKey (Char 'M') Down _ _) (Game (GameState _ _ _ gameRngVal twoPlayerModeFromMenu startPlayer)) =
    Menu (MenuState 0 False gameRngVal twoPlayerModeFromMenu startPlayer)

handleEvent (EventKey (Char 'c') Down _ _) (Menu (MenuState _ _ rngVal _ _)) =
    Credits (initialCreditsState rngVal)
handleEvent (EventKey (Char 'C') Down _ _) (Menu (MenuState _ _ rngVal _ _)) =
    Credits (initialCreditsState rngVal)

handleEvent (EventKey (Char 'r') Down _ _) (Menu (MenuState _ _ rngVal _ _)) =
    Rules (RulesState rngVal)
handleEvent (EventKey (Char 'R') Down _ _) (Menu (MenuState _ _ rngVal _ _)) =
    Rules (RulesState rngVal)

handleEvent (EventKey (Char 'm') Down _ _) (Rules (RulesState rngVal)) =
    Menu (MenuState 0 False rngVal False One)
handleEvent (EventKey (Char 'M') Down _ _) (Rules (RulesState rngVal)) =
    Menu (MenuState 0 False rngVal False One)

handleEvent (EventKey (Char 'm') Down _ _) (Credits (CreditsState _ rngVal)) =
    Menu (MenuState 0 False rngVal False One)
handleEvent (EventKey (Char 'M') Down _ _) (Credits (CreditsState _ rngVal)) =
    Menu (MenuState 0 False rngVal False One)

handleEvent event (Game gState) = Game (handleGameEvent event gState)
handleEvent _ s = s

handleGameEvent :: Event -> GameState -> GameState
handleGameEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) (GameState ttt w randEnabledFromMenu gameRngVal twoPlayerModeVal startPlayer)
  | (currentPlayer ttt == One && not (gameOver ttt)) || (twoPlayerModeVal && not (gameOver ttt)) =
      let x = floor ((mx + 1.5 * fromIntegral w) / fromIntegral w)
          y = 2 - floor ((my + 1.5 * fromIntegral w) / fromIntegral w)
          move = P (y,x)
      in if move `elem` moves ttt
          then GameState (makeMove ttt move) w randEnabledFromMenu gameRngVal twoPlayerModeVal startPlayer
        else GameState ttt w randEnabledFromMenu gameRngVal twoPlayerModeVal startPlayer

handleGameEvent (EventKey (Char 'r') Down _ _) (GameState ttt w randEnabledFromMenu currentRngInGame twoPlayerModeFromMenu startPlayer)
  | gameOver ttt =
    let (resetGame, newRngForReset) = initialGameFromMenu (gameMode ttt) randEnabledFromMenu currentRngInGame twoPlayerModeFromMenu startPlayer
    in GameState resetGame w randEnabledFromMenu newRngForReset twoPlayerModeFromMenu startPlayer
  | otherwise    = GameState ttt w randEnabledFromMenu currentRngInGame twoPlayerModeFromMenu startPlayer

handleGameEvent (EventKey (Char 'R') Down _ _) (GameState ttt w randEnabledFromMenu currentRngInGame twoPlayerModeFromMenu startPlayer)
  | gameOver ttt =
    let (resetGame, newRngForReset) = initialGameFromMenu (gameMode ttt) randEnabledFromMenu currentRngInGame twoPlayerModeFromMenu startPlayer
    in GameState resetGame w randEnabledFromMenu newRngForReset twoPlayerModeFromMenu startPlayer
  | otherwise    = GameState ttt w randEnabledFromMenu currentRngInGame twoPlayerModeFromMenu startPlayer

handleGameEvent _ g = g


update :: Float -> GuiState -> GuiState
update _ (Menu s) = Menu s
update _ g@(Game gState@(GameState ttt w randEnabledFromMenu gameRngVal twoPlayerModeVal startPlayer))
  | not twoPlayerModeVal && currentPlayer ttt == Two && not (gameOver ttt) =
      Game (GameState (makeMove ttt (bestMove ttt)) w randEnabledFromMenu gameRngVal twoPlayerModeVal startPlayer)
  | otherwise = g
update s (Credits cState) = Credits (updateCredits s cState)
update _ (Rules s) = Rules s

updateCredits :: Float -> CreditsState -> CreditsState
updateCredits s (CreditsState symbols rngVal) =
  let
    updatedSymbols = map (updateSymbol s) symbols
    (newSymbols, newRng) = foldl  (\(acc, r) p -> if isOffScreen (symbolPos p) then
                                                  let ((x,y), r1) = randomSpawn r
                                                      ((vx,vy), r2) = randomVel r1
                                                      (charC, r3) = randomR (0,1) r2 :: (Int, StdGen)
                                                      char = if charC == 0 then 'X' else '0'
                                                  in (CreditSymbol (x,y) (vx,vy) char : acc, r3)
                                                else (p : acc, r))
                                  ([], rngVal)
                                  updatedSymbols
  in
    CreditsState newSymbols newRng
  where
    updateSymbol :: Float -> CreditSymbol -> CreditSymbol
    updateSymbol dt (CreditSymbol (x,y) (vx,vy) char) =
      CreditSymbol (x + vx * dt, y + vy * dt) (vx,vy) char

    isOffScreen :: (Float, Float) -> Bool
    isOffScreen (x,y) = abs x > 310 || abs y > 310

    randomSpawn :: StdGen -> ((Float, Float), StdGen)
    randomSpawn r =
      let
        maxPos = 310
        (x,rng2) = randomR (-maxPos, maxPos) r
        (y,rng3) = randomR (-maxPos, maxPos) rng2
      in ((x,y), rng2)

    randomVel :: StdGen -> ((Float, Float), StdGen)
    randomVel r =
      let
        maxSpeed = 50
        (vx, rng4) = randomR (-maxSpeed, maxSpeed) r
        (vy, rng5) = randomR (-maxSpeed, maxSpeed) rng4
      in if vx == 0 && vy == 0
          then randomVel rng4
          else ((vx,vy), rng4)



render :: GuiState -> Picture
render (Menu mState) = renderMenu mState
render (Game gState) = renderTTT gState
render (Credits cState) = renderCredits cState
render (Rules rState) = renderRules rState


main :: IO ()
main = do
  initialRng <- newStdGen

  play
    (InWindow "TicTacToe" (600, 600) (100, 100))
    black
    60 --fps
    (Menu (MenuState 0 False initialRng False One))
    render
    handleEvent
    update
