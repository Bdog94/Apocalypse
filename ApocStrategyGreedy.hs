
module ApocStrategyGreedy where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import ApocTools
import System.Random
import AItools
--import Apoc

--For quick reference..
--type Chooser = GameState -> PlayType -> Player -> IO (Maybe [(Int,Int)])


greedyTest :: IO (Maybe [(Int, Int)])

greedyTest =  greedy initBoard Normal White 
			 
greedyTest2 = validMovesGenPlayer (theBoard initBoard) White (generateMovesForGreedyStrat (theBoard initBoard) ) 



-- |
greedy :: Chooser 
greedy b playType player = determineMove (validMovesGenPlayer (theBoard b) player (generateMovesForGreedyStrat (theBoard b))) b playType player



-- | This function determines the move that the AI will take
determineMove :: [ ((Int , Int) , (Int , Int) )] ->Chooser		

determineMove [] _ _ _ = return (Nothing)
determineMove ((source, dest) : list) b Normal player         | player == White || player == Black
							 = return (Just(pickMoveGreedy(sortMoves( evalMoves ((source,dest) : list) b player ) )))			
determineMove ((source, dest) : list ) b PawnPlacement player  | player == White || player == Black
														 = return (Just([findFirstEmptySpot (theBoard b)]))




-- Probably is not needed but makes it easier to select a move once the list has been sorted...
-- Also allows for a simple front to change if we need to add randomness						
pickMoveGreedy :: [(Int, ((Int, Int), (Int, Int)))] -> [(Int, Int)]

pickMoveGreedy ((val, (source,dest)):xs ) = [(source), (dest)]
 
--Sorts all the move objects       
sortMoves :: [(Int, ((Int, Int), (Int, Int)))] -> [(Int, ((Int,Int), (Int, Int)) )]
sortMoves [] = []
sortMoves ((val,(dest)):xs) = (sortMoves greater) ++ [(val,(dest))] ++ (sortMoves lesser)
     where
          lesser  = filter ( lessComparator    val) xs
          greater = filter ( greaterComparator val) xs	
          

-- similar to >= val, the second value is the wrapper structure for a move
greaterComparator :: Int-> (Int, ((Int, Int), (Int, Int))) -> Bool

greaterComparator val (val_1, ( _ , _))  = if val_1 >= val
                                          then True
                                          else False
    
-- similar to < val, the second value is the wrapper structure for a move                                      
lessComparator :: Int-> (Int, ((Int, Int), (Int, Int))) -> Bool

lessComparator val (val_1, ( _ , _))  = if val_1 >= val
                                          then False
                                          else True                



--The first int in the last part is the score the second last is the destination
evalMoves ::  [ ((Int , Int) , (Int , Int) )] -> GameState -> Player -> [(Int, ((Int, Int), (Int, Int)))] 

evalMoves [] _ _ = []
evalMoves ((source, dest):xs) game p =  [(evalMove (source,dest) game p, (source, dest))] ++ evalMoves xs game p

evalMove  ::  ((Int , Int) , (Int , Int))    -> GameState -> Player -> Int
evalMove  ( (a,b), (c,4)) game White  = 5
evalMove  ( (a,b), (c,0)) game Black  = 5
evalMove  ( (a,b), (c,d)) game player =  scoreMove player (cell2Char((getFromBoard (theBoard game) (c, d))))

scoreMove :: Player -> Char -> Int

scoreMove  Black 'X' = 2	--Black is taking out a White Knight
scoreMove  White '#' = 2	--White is taking out a Black Knight
scoreMove  Black '/' = 1    	--Black is taking out a White Pawn
scoreMove  White '+' = 1 	--White is taking out a Black Pawn
scoreMove  Black  c  = 0		 
scoreMove  White  c  = 0



rand = do
    return=<<randomRIO(0, 4 :: Int)
    
randomGen :: IO [Int]
randomGen = do
    let randArr = [] :: [Int]
    randOne <- rand
    randTwo <- rand
    randThree <- rand
    randFour <- rand
    
    return (randOne : randTwo: randThree: randFour : randArr)
    
    

zeroToNum :: Int -> IO Int
zeroToNum num = getStdRandom $ randomR (0, num)



