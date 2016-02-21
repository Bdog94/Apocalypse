
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
{-
 _ _ _ _ _
|X|/|/|+|_|
|/|/|_|_|_|
|_|_|_|_|_|
|+|_|#|_|+|
|#|+|+|_|X|


 _ _ _ _ _
|X|/|/|_|+|
|/|/|_|_|_|
|_|_|_|_|_|
|+|_|#|_|+|
|#|+|+|_|X|

 _ _ _ _ _
|_|_|/|#|_|
|/|_|X|_|_|
|_|/|_|X|_|
|+|+|+|_|+|
|#|_|_|_|_|

-}

greedyTest1       :: GameState
greedyTest1 = GameState Init 0 Init 0
                          [ [WK, WP, WP, BP, E],
                          [WP, WP , E , E , E],
                          [E , E , E , E , E ],
                          [BP, E , BK , E , BP],
                          [BK, BP, BP,  E , WK] ]
                          
greedyTestBoard2       :: GameState
greedyTestBoard2 = GameState Init 0 Init 0
                          [ [WK, WP, WP, E, BP],
                          [WP, WP , E , E , E ],
                          [E , E , E , E ,  E ],
                          [BP, E , BK , E ,  BP],
                          [BK, BP, BP,  E , WK] ]
                          
greedyTestBoard3       :: GameState
greedyTestBoard3  = GameState Init 0 Init 0
                          [ [E, E , WP, BK, E ],
                          [WP, E , WK , E , E ],
                          [E , WP , E , WK, E ],
                          [BP, BP, BP , E , BP],
                          [BK, E , E ,  E , E ] ]                          
                          
                          



greedyTest :: IO (Maybe [(Int, Int)])

greedyTest =  greedy initBoard Normal White 
			 
greedyTest2 = validMovesGenPlayer (theBoard initBoard) White (generateMovesForGreedyStrat (theBoard initBoard) ) 



-- |
greedy :: Chooser 
greedy b playType player = determineMove (validMovesGenPlayer (theBoard b) player (generateMovesForGreedyStrat (theBoard b))) b playType player



-- | This function determines the move that the AI will take
determineMove :: [ ((Int , Int) , (Int , Int) )] ->Chooser		

determineMove ((source, dest) : list) b Normal player         | player == White || player == Black
							 = pickMoveGreedy(sortMoves( evalMoves ((source,dest) : list) b player ) )
determineMove [(source, dest)] b Normal player | player == White 	|| player == Black
							= return( Just ([(source), (dest)]))		
determineMove ((source, dest) : list ) b PawnPlacement player  | player == White || player == Black
														 =  chosePawn ( generateAllEmptyMoves (theBoard b) )
determineMove [] b PawnPlacement _ =  chosePawn ( generateAllEmptyMoves (theBoard b) )
determineMove [] b Normal player = return(Nothing)




-- Probably is not needed but makes it easier to select a move once the list has been sorted...
-- Also allows for a simple front to change if we need to add randomness						
pickMoveGreedy :: [(Int, ((Int, Int), (Int, Int)))] -> IO (Maybe [(Int, Int)])

pickMoveGreedy [(val, (source, dest))] = return( Just ( [(source),  (dest)]))
pickMoveGreedy ((val_1, (source_1,dest_1)) : (val_2, (source_2, dest_2)) :xs ) = do 
   	  int <- randomRIO (0 , 9)
   	  if greedyComparator int
   	  then return( Just([(source_2),  (dest_2)]))
   	  else return( Just([(source_1),  (dest_1)]))

 
greedyComparator :: Int -> Bool

greedyComparator num = if (num >= 9) 
					   then True
					   else False
 
 
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

evalMove  ( (a,b), (c,d)) game player =  scoreMove player (cell2Char((getFromBoard (theBoard game) (c, d))))

scoreMove :: Player -> Char -> Int

scoreMove  Black 'X' = 5	--Black is taking out a White Knight
scoreMove  White '#' = 5	--White is taking out a Black Knight
scoreMove  Black '/' = 4    --Black is taking out a White Pawn
scoreMove  White '+' = 4 	--White is taking out a Black Pawn
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



