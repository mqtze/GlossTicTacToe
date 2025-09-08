
> {-# LANGUAGE MultiParamTypeClasses #-}
>
> module StrategicGames where


> import Debug.Trace
> import Data.List(maximumBy,transpose)
> import Data.List.Lens
> import Control.Lens.Combinators
> import Control.Lens.Operators
> import Control.Parallel.Strategies
> --import Data.List.Utils
> import Data.List.Extra
> import Control.Exception
> import Control.Monad
  
> import Data.Typeable

> data Player = One  |Two  | None deriving (Eq)

> nextPlayer One = Two                                          
> nextPlayer Two = One                                                 

> instance Show Player where
>  show One = "X"
>  show Two = "0"
>  show _   = " "

> data GameMode = Normal | Misere | Notakto | PieceLimit deriving (Eq, Show)

> class Show m => Game g m where

>   start :: (g m) 

>   currentPlayer :: (g m)  -> Player

>   moves ::  (g m)  -> [m]

>   makeMove :: g m -> m -> g m

>   hasWinner :: g m -> Bool

>   gameOver :: g m -> Bool
>   gameOver g  = null (moves g)||hasWinner g

>   score :: g m -> Int -> Int
>   score g movesUsed
>     |hasWinner g = case gameMode g of 
>                       Normal -> -(winscore g) + movesUsed
>                       Misere -> (winscore g) - movesUsed
>                       Notakto -> (winscore g) - movesUsed
>                       PieceLimit -> -(winscore g) + movesUsed
>     |otherwise = 0

>   winscore :: g m -> Int

>   winscore g = 1000000


>   searchDepth :: g m -> Int
>   searchDepth g = 5

>   ai :: g m ->  AI (g m) 
>   ai _ = alphaBeta

>   evalMoves :: g m -> [(m,Int)]
>   evalMoves g = parMap
>    (evalTuple2 r0 rdeepseq)
>    (\m-> (m, (ai g) (searchDepth g) (makeMove g m)))
>    (moves g)

>   bestMove ::  g m -> m
>   bestMove g =
>     let x = fst
>                    $ maximumBy (\(_,v1) (_,v2) ->if v1<v2 then LT else GT)
>                    $ evalMoves  g
>     in x

>   gameMode :: g m -> GameMode
>   setSqares :: g m -> Int

> type  AI a = Int -> a -> Int

> data Tree a = Node a [Tree a]  deriving Show

> createGameTree g = 
>   Node g [createGameTree (makeMove g m)  |m <- moves g]

> minimax depth g = -(minimax' depth $ createGameTree  g)
>  where

>   minimax' _ (Node g [])       = score g depth
>   minimax' 0 (Node g _)        = score g depth

>   minimax' d (Node _ cs)
>      = maximum $ map (negate . (minimax' d-1)) $ cs

> alphaBeta depth g
>     = -alphaBeta' depth 0 (-ws) (ws) (createGameTree g)
>  where
>    ws =  winscore g
>
>    alphaBeta' 0 moves _     _    (Node g _)  = score g moves
>    alphaBeta' d moves _     _    (Node g []) = score g moves
>    alphaBeta' d moves alpha beta (Node _ cs) = falte (alpha,beta) crs
>      where
>        crs = map (\c-> -(alphaBeta' (d-1)(moves+1)(-beta)(-alpha) c)) cs
> 
>        falte ab@(alpha,beta) (wert:werte)
>          |wert >= beta = beta
>          |wert > alpha = falte (wert,beta) werte
>          |otherwise    = falte ab werte
>        falte (alpha,_) [] = alpha

> playUserMove a = 
>   catch
>     (do
>        print a
>        putStrLn "Geben Sie jetzt Ihren Zug ein!"
>        putStrLn "Mögliche Züge:"
>        sequence $ map (putStrLn.show) $ moves a
>        line <- getLine
>        let a1 = makeMove a $ read line
>        print a1
>        return a1
>     )
>     (\e-> do
>       print (e :: SomeException)
>       playUserMove a
>     ) 

> playAIMove a = do
>   putStrLn "Nun überlege ich meinen Zug.."
>   return $ makeMove a $ bestMove a  

> playGame a   
>   |gameOver a = putStrLn "Das war ein Remis."
> playGame a = do
>   a1 <- playUserMove a 
>   if (hasWinner a1) then putStrLn "Gratulation! Sie haben gewonnen."
>   else if not $ gameOver a1
>   then do 
>     a2 <- playAIMove  a1
>     if (hasWinner a2)
>     then do
>        print a2
>        putStrLn "Dieses Spiel hat der Rechner gewonnen."
>     else playGame a2
>   else playGame a1


> data TicTacToe a = TTT Player [Player] GameMode Bool [P] Bool -- spieler, board, gamemode, randomstart, list of last moves for piecelimit, twoplayermode

> instance Show (TicTacToe m)  where
>   show (TTT npl board _ randomEnabled _ twoPlayerMode) =
>       "Aktueller Spieler: " ++ show npl ++ "\n" ++
>       "Spielbrett:\n" ++
>       showBoard board ++
>       (if randomEnabled then "\n(Random Start Aktiv)" else "")
>       where
>           showBoard :: [Player] -> String
>           showBoard board =
>               let row1 = take 3 board
>                   row2 = take 3 (drop 3 board)
>                   row3 = take 3 (drop 6 board)
>               in
>                   showRow row1 ++ "\n" ++
>                   "-----------\n" ++
>                   showRow row2 ++ "\n" ++
>                   "-----------\n" ++
>                   showRow row3

>           showRow :: [Player] -> String
>           showRow [a,b,c] = " " ++ show a ++ " | " ++ show b ++ " | " ++  show c ++ " "

> data InvalidMoveException = InvalidMoveException String deriving (Show, Typeable)
> instance Exception InvalidMoveException

> newtype P = P(Int, Int) deriving (Show,Read,Eq)

> instance Game TicTacToe P  where

>  start = TTT One [None, None, None,None, None, None, None, None, None] Normal False [] False

>  currentPlayer (TTT player _ _ _ _ _) = player

>  moves g@(TTT _ board _ _ _ _) 
>   | hasWinner g   = []
>   | otherwise     = [P (div i 3, mod i 3) | (i,p)<- zip [0..] board, p == None]

>  makeMove g@(TTT player board mode randomEnabled placedPieces twoPlayerMode) m@(P(x,y)) =

>    let index = 3*x + y
>        boardAfterPlacement = take index board ++ [if mode == Notakto then One else player] ++ drop (index+1) board
>        (finalBoard, updatedPlacedPieces) = case mode of
>            PieceLimit ->
>                if length placedPieces >= 6
>                then
>                    let P (oldX, oldY) = head placedPieces
>                        oldIndex = 3 * oldX + oldY
>                        boardWithoutOldest = take oldIndex boardAfterPlacement ++ [None] ++ drop (oldIndex + 1) boardAfterPlacement
>                    in
>                        (boardWithoutOldest, tail placedPieces ++ [m])
>                else
>                    (boardAfterPlacement, placedPieces ++ [m])
>            _ -> (boardAfterPlacement, []) --für andere modi wird "letzte Symbole" nicht gebraucht
>
>    in if elem m (moves g) then TTT (nextPlayer player) finalBoard mode randomEnabled updatedPlacedPieces twoPlayerMode else throw (InvalidMoveException "Kein erlaubter Zug!")



>  hasWinner (TTT _ board _ _ _ _) = any isWin winners
>   where
>       isWin [a,b,c] = a/= None && a == b && a == c
>       winners = [[board !! i | i <- line] | line <- 
>                   [[0,1,2], [3,4,5], [6,7,8],
>                    [0,3,6], [1,4,7], [2,5,8],
>                    [0,4,8],[2,4,6]]]

>  gameMode (TTT _ _ mode _ _ _) = mode

>  setSqares (TTT _ board _ _ _ _) = 9 - length xs
>       where 
>           xs = [xs | xs <- board, xs == None]

> playTicTacToe = playGame (start :: TicTacToe P)
