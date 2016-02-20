module ApocStrategyRandom where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import ApocTools
import System.Random
import AItools

{-

 _ _ _ _ _
|_|_|/|/|X|
|/|#|X|_|_|
|+|_|_|#|_|
|_|_|_|_|_|
|_|+|+|/|_|

-}

randomTest1       :: GameState
randomTest1 = GameState Init 0 Init 0
                          [ [E, E, WP, WP, WK],
                          [WP, BK , WK , E, E],
                          [BP, E , E , BK , E],
                          [E , E , E , E  , E],
                          [E , BP, BP,  WP, E] ]



randomStrategy :: Chooser

randomStrategy b Normal        player = pickRandomMoveNormal b player
randomStrategy b PawnPlacement player = pickRandomMovePawn (theBoard b) player

pickRandomMoveNormal :: GameState -> Player -> IO(Maybe [(Int, Int)])

pickRandomMoveNormal b player = choseRandom ( validMovesGenPlayer (theBoard b) player (generateMovesForGreedyStrat (theBoard b)) ) 

pickRandomMovePawn b player = chosePawn ( generateAllEmptyMoves b )




choseRandom :: [((Int,Int) , (Int, Int))] -> IO (Maybe [(Int,Int)])
choseRandom []   = return(Nothing)
choseRandom list = do 
   	  int <- randomRIO (0 , (length list -1))
   	  return (Just (getNormalMoveAtIndex list int))
   --if int == 0 
   --then return [[source] , [dest]]
   --else return (getNormalMoveAtIndex ((source:dest):xs) int)

useInt :: Int -> Int 
useInt x = x+10



getNormalMoveAtIndex :: [((Int , Int), (Int, Int))] -> Int -> [(Int, Int)]

getNormalMoveAtIndex [] _ = []
getNormalMoveAtIndex [(source, dest)] _ = [(source), (dest)]
getNormalMoveAtIndex ((source, dest) : list) 0 = [(source), (dest)]
getNormalMoveAtIndex ((source, dest) : list) x = getNormalMoveAtIndex list (x - 1)
