{- | This module is used for CPSC 449 for the Apocalypse assignment.
     This module is where the functions related to the Artificial Intelligence sit
-}
module AItools where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import ApocTools
import System.Random

-- | chosePawn is a function that randomly chooses any pawn within a list

chosePawn :: [ (Int,Int)] -- list to search through
          ->IO (Maybe [(Int,Int)]) -- ^ return index of pawn

chosePawn [] = return(Nothing)
chosePawn list = do
			int <- randomRIO (0, (length list - 1))
			return(Just(getPawnMoveAtIndex list int))
	
	
-- | given a list of pawn moves and an index, getPawnMoveAtIndex is a function that returns the pawn that is at the pawn move at the index given	
getPawnMoveAtIndex   :: [(Int, Int)] -- ^ List of pawn moves
                     -> Int -- ^ Index given to us
                     -> [(Int, Int)] -- ^ return pawn
getPawnMoveAtIndex [] _ = []
getPawnMoveAtIndex [(x,y)] _ = [(x,y)]
getPawnMoveAtIndex ((x,y): list) 0 = [(x,y)]
getPawnMoveAtIndex ((x,y): list) index = getPawnMoveAtIndex list (index - 1)


-- | zeroToNum is a function that generates a random integer from 0, to the parameter that is given
zeroToNum :: Int -- ^ highest number that the randomly generated number can be
          -> IO Int -- ^ returned random integer
zeroToNum num = getStdRandom $ randomR (0, num)


-- | findFirstEmptySpot is a function that will search the game board and return the first empty spot
findFirstEmptySpot :: Board -- ^ Initial Game Board
                   -> (Int, Int) -- ^ returned coordinates of first empty spot
findFirstEmptySpot b = indexOfEmptySpot (formatStringForGreedy (board2Str b)) 0


-- | indexOfEmptySpot is a function that returns the index of an empty spot 
indexOfEmptySpot :: String -- ^ list that is given to us
                    -> Int -- ^ slot number
                    -> (Int,Int) -- ^ returned index of the empty spot

indexOfEmptySpot list    slot | slot > 25  || slot < 0= (0, 0)
indexOfEmptySpot (x :xs) slot = if searchForEmptyChar x 
								then ( div slot  5,  mod slot  5)
								else indexOfEmptySpot xs (slot + 1)

-- | searchForEmptyChar is a function that checks if a char given as a parameter is an empty spot char within a gameboard
searchForEmptyChar :: Char -- ^ given char
                   -> Bool -- ^ True if == '_', otherwise false

searchForEmptyChar '_' = True
searchForEmptyChar _   = False


-- | generateAllEmptyMoves is a function that generates all of the possible moves into empty spots
generateAllEmptyMoves :: Board -- ^ Initial Game Board
                      -> [(Int, Int)] -- ^ return list of moves

generateAllEmptyMoves b = generateAllEmptyMoves' (formatStringForGreedy (board2Str b)) 0

-- | generateAllEmptyMoves' is a function that generates all of the possible moves into empty slots
generateAllEmptyMoves' :: String -- ^ List to go through
                          -> Int -- ^ Index
                          -> [(Int,Int)] -- ^ generated move

generateAllEmptyMoves' [] index = []
generateAllEmptyMoves' _ index | index > 25 || index < 0 = []
generateAllEmptyMoves' (x:xs) index = if x == '_' 
									  then [(mod index 5, div index 5)] ++ generateAllEmptyMoves' xs (index +1) 
									  else generateAllEmptyMoves' xs (index + 1)


-- | Similar to validMovesGenerator but is used in conjunction with the player
validMovesGenPlayer :: Board -- ^ Initial Game Board
                    -> Player -- ^ Player
                    -> [ ((Int, Int), (Int, Int))] -- ^ source and destination
                    -> [ ((Int, Int) , (Int , Int))] -- ^ returned list of moves

validMovesGenPlayer theBoard player [] = []
validMovesGenPlayer theBoard player ((source, dest) :xs) = if isValidForPlayer theBoard player ([(source)] ++ [(dest)])
														   then [(source, dest)] ++ validMovesGenPlayer theBoard player xs
														   else validMovesGenPlayer theBoard player xs
-- | Additional check to ensure a player can only move their own piece
isValidForPlayer :: Board -- ^ Initial Game Board
                 -> Player -- ^ Player
                 -> [(Int,Int)] -- ^ list of moves
                 -> Bool -- ^ True if player can move that piece
isValidForPlayer board player [] = False
isValidForPlayer board player (first:rest) | (getFromBoard board first) == E = False
										   | (player == playerOf (pieceOf(getFromBoard board first)))  = isValidMove board first (head rest)
                                           | True = False
                                              

-- | isValidMove is a function that takes a move, and checks to see if that move is valid within that gameboard
isValidMove :: Board -- ^ Initial Game Board
            -> (Int,Int) -- ^ Initial move coordinates
            -> (Int,Int) -- ^ Final move coordinates
            -> Bool -- ^ return True if valid, False otherwise
isValidMove theBoard (x,y) (w,z)
    | (x<0) || (x>4)|| (y<0) || (y>4) || (w<0) || (w>4) || (z<0) || (z>4) = False
    |((getFromBoard theBoard (x,y)) == WK ) =               --  A knights move is valid if it:
        if (((abs (x-w))<3) && ((abs (y-z)))<3)             --  moves 2 spaces on one axis, and one space on the other 
           && (((abs (x-w)) + (abs (y-z))) == 3)            --  (totaling 3 spaces)
           && (((getFromBoard theBoard (w,z)) ==  E)        --  and if the target space is empty 
               || ((getFromBoard theBoard (w,z)) ==  BK)    --  or contains an opponent's piece.
               || ((getFromBoard theBoard (w,z)) ==  BP))
        then True
        else False

    |((getFromBoard theBoard (x,y)) == BK ) = 
        if (((abs (x-w))<3) && ((abs (y-z)))<3)
           && (((abs (x-w)) + (abs (y-z))) == 3)
           && (((getFromBoard theBoard (w,z)) ==  E)
               || ((getFromBoard theBoard (w,z)) ==  WK)
               || ((getFromBoard theBoard (w,z)) ==  WP))
        then True
        else False
 --  A pawns move is valid if it:
 --  moves one space vertically into an open space
 --  or move diagonally one space onto a space 
 --  occupied by an opponent's piece
    |((getFromBoard theBoard (x,y)) == WP ) && (((abs (x-w)) == 0) && ((y-z) == -1) && getFromBoard theBoard (w,z) == E)           
        = True                                           
    |((getFromBoard theBoard (x,y)) == WP ) && (((abs (x-w)) == 1) && ((y-z) == -1)) 
	&& (((getFromBoard theBoard (w,z)) ==  BK) || ((getFromBoard theBoard (w,z)) ==  BP))
             = True
    |((getFromBoard theBoard (x,y)) == BP ) && (((abs (x-w)) == 0) && ((y-z) == 1) && getFromBoard theBoard (w,z) == E)           
        = True                                           
    |((getFromBoard theBoard (x,y)) == BP ) && (((abs (x-w)) == 1) && ((y-z) == 1)) 
	&& (((getFromBoard theBoard (w,z)) ==  WK) || ((getFromBoard theBoard (w,z)) ==  WP))
             = True
    |True = False


-- | formatStringForGreedy is a function that generates an output string for the greedy player strategy
formatStringForGreedy :: String -- ^ Initial String
                      -> String -- ^ Final String
formatStringForGreedy s = filter helperFormatStringForGreedy (removeFront s)

-- | Removes " _ _ _ _ _\n" from the String
removeFront :: String -- ^ Initial String
            -> String -- ^ return final string with no "_ _ _ _ _\n"
removeFront (x:xs) | x == '\n'  && length xs  >= 0= [] ++ xs
removeFront (x:xs) = removeFront xs 


-- | helperFormatStringForGreedy is a function that takes a char as a parameter, and returns true if that char is not "|" or "\n"

helperFormatStringForGreedy :: Char -- ^ Given Char
                            -> Bool -- ^ True if not "\n" or "|"
helperFormatStringForGreedy '|' = False
helperFormatStringForGreedy '\n' = False
helperFormatStringForGreedy  c = True


-- | generateMovesForGreedyStrat is a function that generates all possible moves that a greedy player can make
generateMovesForGreedyStrat :: Board -- ^ Initial Game Board
                               -> [ ((Int, Int), (Int, Int))] -- ^ List of possible moves

generateMovesForGreedyStrat b = generateMovesForGreedyStratString (formatStringForGreedy (board2Str b)) 0

-- | generateMovesForGreedyStratString is a function that generates all possible moves that a greedy player can make using a string
generateMovesForGreedyStratString :: String -- ^ String
                                  -> Int  -- ^ index
                                  -> [((Int, Int) , (Int, Int))] -- ^ List of possible moves
generateMovesForGreedyStratString [] _ = []

generateMovesForGreedyStratString (c: cs) num |  (num) > 25 || (num) < 0 = []

generateMovesForGreedyStratString (c: cs) num = generateMovesForGreedyStratUsingChar c ( mod num 5, div num 5) ++ generateMovesForGreedyStratString cs (num + 1)

-- |  generateMovesForGreedyStratUsingChar is a function that generates all possible moves that a greedy player can make using a char             
generateMovesForGreedyStratUsingChar :: Char -- ^ Given char
                                     -> (Int, Int) -- ^ coordinates of piece
                                     -> [ ((Int, Int), (Int, Int))] -- ^ return list of possible moves

generateMovesForGreedyStratUsingChar c (x,y) | c == '_' = []
generateMovesForGreedyStratUsingChar c (x,y) | c == '/' || c == '+' = [((x,y), (x  , y+1))]++ [((x,y), (x +1 , y +1))] ++ [((x,y), (x+1, y))] ++ [((x,y) , (x -1, y +1))] ++
                                            						  [((x,y), (x  , y-1))]++ [((x,y), (x -1 , y -1))] ++ [((x,y), (x-1,y ))] ++ [((x,y) , (x +1, y -1))]
generateMovesForGreedyStratUsingChar c (x,y) | c == 'X' || c == '#' = [ ((x,y) , (x - 2, y + 1))] ++ [((x,y) , (x-1, y+2))] ++ [((x,y) , (x+1, y+2))] ++ [((x, y) , (x+2, y+1))] ++
                                                                      [ ((x,y) , (x + 2, y - 1))] ++ [((x,y) , (x+1, y-2))] ++ [((x,y) , (x-1, y-2))] ++ [((x, y) , (x-2, y-1))] 

    


    
                                        
