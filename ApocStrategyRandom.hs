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

-- | Test GameState
randomTest1       :: GameState -- ^ Data structure used to store the gamestate information
randomTest1 = GameState Init 0 Init 0
                          [ [E, E, WP, WP, WK],
                          [WP, BK , WK , E, E],
                          [BP, E , E , BK , E],
                          [E , E , E , E  , E],
                          [E , BP, BP,  WP, E] ]


-- | The chooser that is used for the RandomStrategy
randomStrategy :: Chooser -- ^ Data structure that contains information about the gamestate, playtype and the Player

randomStrategy b Normal        player = pickRandomMoveNormal b player
randomStrategy b PawnPlacement player = pickRandomMovePawn (theBoard b) player

--| function to return the random move the AI
pickRandomMoveNormal :: GameState -- ^ Data structure used to store the gamestate information
						-> Player -- ^ The Player to choose the move for
						-> IO(Maybe [(Int, Int)]) -- ^ Data structure used to return the move

pickRandomMoveNormal b player = choseRandom ( validMovesGenPlayer (theBoard b) player (generateMovesForGreedyStrat (theBoard b)) ) 

pickRandomMovePawn b player = chosePawn ( generateAllEmptyMoves b )



--| Method that actually picks the random move
choseRandom :: [((Int,Int) , (Int, Int))] -- ^List of moves in ((source), (dest)) form
			    -> IO (Maybe [(Int,Int)]) -- ^ Data structure used to return the move
choseRandom []   = return(Nothing)
choseRandom list = do 
   	  int <- randomRIO (0 , (length list -1))
   	  return (Just (getNormalMoveAtIndex list int))



--| Used to get the move at a certain index in the moves list
getNormalMoveAtIndex :: [((Int , Int), (Int, Int))] -- ^List of moves in ((source), (dest)) form
						-> Int						-- ^Index of the move to be chosen
						-> [(Int, Int)]				-- ^The move that is to be returned

getNormalMoveAtIndex [] _ = []
getNormalMoveAtIndex [(source, dest)] _ = [(source), (dest)]
getNormalMoveAtIndex ((source, dest) : list) 0 = [(source), (dest)]
getNormalMoveAtIndex ((source, dest) : list) x = getNormalMoveAtIndex list (x - 1)
