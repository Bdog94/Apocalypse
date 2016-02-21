{- | Module: ApocStrategyGreedy.hs

Implements the functionality of the greedy strategy.
Use the greedy :: Chooser method to get the move for the AI

Created for the CPSC 449 haskell assignment 

-}
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

-- | A test gamestate
greedyTest1       :: GameState -- ^ Data structure used to store the gamestate information
greedyTest1 = GameState Init 0 Init 0
                          [ [WK, WP, WP, BP, E],
                          [WP, WP , E , E , E],
                          [E , E , E , E , E ],
                          [BP, E , BK , E , BP],
                          [BK, BP, BP,  E , WK] ]
-- | A test gamestate                          
greedyTestBoard2       :: GameState -- ^ Data structure used to store the gamestate information
greedyTestBoard2 = GameState Init 0 Init 0
                          [ [WK, WP, WP, E, BP],
                          [WP, WP , E , E , E ],
                          [E , E , E , E ,  E ],
                          [BP, E , BK , E ,  BP],
                          [BK, BP, BP,  E , WK] ]
       
-- | A test gamestate                          
greedyTestBoard3       :: GameState  -- ^ Data structure used to store the gamestate information
greedyTestBoard3  = GameState Init 0 Init 0
                          [ [E, E , WP, BK, E ],
                          [WP, E , WK , E , E ],
                          [E , WP , E , WK, E ],
                          [BP, BP, BP , E , BP],
                          [BK, E , E ,  E , E ] ]                          
                          
                          

-- | A test that will see what move the AI will take on the initial board
greedyTest :: IO (Maybe [(Int, Int)]) -- ^ The move thae AI will take

greedyTest =  greedy initBoard Normal White 

-- | function used to see all the valid moves for White on the initBoard	 
greedyTest2 = validMovesGenPlayer (theBoard initBoard) White (generateMovesForGreedyStrat (theBoard initBoard) ) 



-- | function that is used to determine the move the greedy AI will take
greedy :: Chooser -- ^ Data structure that contains information about the gamestate, playtype and the Player
greedy b playType player = determineMove (validMovesGenPlayer (theBoard b) player (generateMovesForGreedyStrat (theBoard b))) b playType player



-- | This function determines the move that the AI will take
determineMove :: [ ((Int , Int) , (Int , Int) )] -- ^ A list of the moves which are assumed to be valid in (source), (destination) form
				 ->Chooser	 -- ^ Data structure that contains information about the gamestate, playtype and the Player	

determineMove ((source, dest) : list) b Normal player         | player == White || player == Black
							 = pickMoveGreedy(sortMoves( evalMoves ((source,dest) : list) b player ) )
determineMove [(source, dest)] b Normal player | player == White 	|| player == Black
							= return( Just ([(source), (dest)]))		
determineMove ((source, dest) : list ) b PawnPlacement player  | player == White || player == Black
														 =  chosePawn ( generateAllEmptyMoves (theBoard b) )
determineMove [] b PawnPlacement _ =  chosePawn ( generateAllEmptyMoves (theBoard b) )
determineMove [] b Normal player = return(Nothing)




-- | This is the method that will actually decide which move the greedy AI will take						
pickMoveGreedy :: [(Int, ((Int, Int), (Int, Int)))] -- ^The list of moves in (value , ((source), (dest))) form 
					-> IO (Maybe [(Int, Int)])	-- ^The data structure used to return the move selected

pickMoveGreedy [(val, (source, dest))] = return( Just ( [(source),  (dest)]))
pickMoveGreedy ((val_1, (source_1,dest_1)) : (val_2, (source_2, dest_2)) :xs ) = do 
   	  int <- randomRIO (0 , 9)
   	  if greedyComparator int
   	  then return( Just([(source_2),  (dest_2)]))
   	  else return( Just([(source_1),  (dest_1)]))

 
-- | Allow for a small amount of randomness in the greedy AI
greedyComparator :: Int -- ^ A random number
				 -> Bool -- ^ Whether that number would allow for a true or false result

greedyComparator num = if (num >= 9) 
					   then True
					   else False
 
 
-- | Sorts the moves into descending order       
sortMoves :: [(Int, ((Int, Int), (Int, Int)))] -- ^The unsorted list of moves in (value , ((source), (dest))) form 
			-> [(Int, ((Int,Int), (Int, Int)) )]-- ^The sorted list of moves in (value , ((source), (dest))) form 
sortMoves [] = []
sortMoves ((val,(dest)):xs) = (sortMoves greater) ++ [(val,(dest))] ++ (sortMoves lesser)
     where
          lesser  = filter ( lessComparator    val) xs
          greater = filter ( greaterComparator val) xs	
          

-- | checks if a value of the pivot is greater than the value of the move
greaterComparator :: Int -- ^ Value of a move (usually the pivot)
					-> (Int, ((Int, Int), (Int, Int))) -- ^ The move that has been sent in, in (value, ((source), (dest)) form
					-> Bool -- ^ is true if the value of the pivot is greater the move value

greaterComparator val (val_1, ( _ , _))  = if val_1 >= val
                                          then True
                                          else False
    
-- | checks if a value of the pivot is less than the value of the move                                    
lessComparator :: Int -- ^ Value of a move (usually the pivot)
				  -> (Int, ((Int, Int), (Int, Int))) -- ^ The move that has been sent in, in (value, ((source), (dest)) form
				  -> Bool -- ^ is true if the value of the pivot is less than the move value

lessComparator val (val_1, ( _ , _))  = if val_1 >= val
                                          then False
                                          else True                



-- | This function goes through every move and gives it a score
evalMoves ::  [ ((Int , Int) , (Int , Int) )] -- ^ The list of moves in ((source), (destination)) form
			  -> GameState					  -- ^ Data structure used to store the gamestate information
			  -> Player						  -- ^ The Player that is taking the move
			  -> [(Int, ((Int, Int), (Int, Int)))] -- ^ List of moves in (value, ((source), (dest)) form

evalMoves [] _ _ = []
evalMoves ((source, dest):xs) game p =  [(evalMove (source,dest) game p, (source, dest))] ++ evalMoves xs game p

-- | Function that will evaluate a single move
evalMove  ::  ((Int , Int) , (Int , Int)) 
			   -> GameState -- ^ Data structure used to store the gamestate information
			   -> Player    -- ^ The Player that is taking the move
			   -> Int		-- ^ The value of the move

evalMove  ( (a,b), (c,d)) game player =  scoreMove player (cell2Char((getFromBoard (theBoard game) (c, d))))

-- | Function that gives a score based on the character and the player
scoreMove :: Player -- ^ The Player that is taking the move
			-> Char -- ^ The character that the move would take the piece to
			-> Int  -- ^ Score given for moving to that destination

scoreMove  Black 'X' = 5	--Black is taking out a White Knight
scoreMove  White '#' = 5	--White is taking out a Black Knight
scoreMove  Black '/' = 4    --Black is taking out a White Pawn
scoreMove  White '+' = 4 	--White is taking out a Black Pawn
scoreMove  Black  c  = 0		 
scoreMove  White  c  = 0



-- | randomly generates numbers
rand = do
    return=<<randomRIO(0, 4 :: Int)


-- | randomly generates 4 ints  
randomGen :: IO [Int] -- ^ A list of 4 random ints
randomGen = do
    let randArr = [] :: [Int]
    randOne <- rand
    randTwo <- rand
    randThree <- rand
    randFour <- rand
    
    return (randOne : randTwo: randThree: randFour : randArr)
    
    
-- | function that returns a random number from 0 to a number
zeroToNum :: Int -- ^The highest the random number can be
			  -> IO Int -- ^ A number between 0 to the num that is passed in
zeroToNum num = getStdRandom $ randomR (0, num)



