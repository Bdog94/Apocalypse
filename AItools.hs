module AItools where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import ApocTools
import System.Random

chosePawn :: [ (Int,Int)] ->IO (Maybe [(Int,Int)])

chosePawn [] = return(Nothing)
chosePawn list = do
			int <- randomRIO (0, (length list - 1))
			return(Just(getPawnMoveAtIndex list int))
			
getPawnMoveAtIndex   :: [(Int, Int)] -> Int -> [(Int, Int)]
getPawnMoveAtIndex [] _ = []
getPawnMoveAtIndex [(x,y)] _ = [(x,y)]
getPawnMoveAtIndex ((x,y): list) 0 = [(x,y)]
getPawnMoveAtIndex ((x,y): list) index = getPawnMoveAtIndex list (index - 1)

zeroToNum :: Int -> IO Int
zeroToNum num = getStdRandom $ randomR (0, num)


-- |
findFirstEmptySpot :: Board -> (Int, Int)

findFirstEmptySpot b = indexOfEmptySpot (formatStringForGreedy (board2Str b)) 0


indexOfEmptySpot :: String -> Int -> (Int,Int)

indexOfEmptySpot list    slot | slot > 25  || slot < 0= (0, 0)
indexOfEmptySpot (x :xs) slot = if searchForEmptyChar x 
								then ( div slot  5,  mod slot  5)
								else indexOfEmptySpot xs (slot + 1)

searchForEmptyChar :: Char -> Bool

searchForEmptyChar '_' = True
searchForEmptyChar _   = False


generateAllEmptyMoves :: Board -> [(Int, Int)]

generateAllEmptyMoves b = generateAllEmptyMoves' (formatStringForGreedy (board2Str b)) 0


generateAllEmptyMoves' :: String -> Int -> [(Int,Int)]

generateAllEmptyMoves' [] index = []
generateAllEmptyMoves' _ index | index > 25 || index < 0 = []
generateAllEmptyMoves' (x:xs) index = if x == '_' 
									  then [(mod index 5, div index 5)] ++ generateAllEmptyMoves' xs (index +1) 
									  else generateAllEmptyMoves' xs (index + 1)


-- | Similar to validMovesGenerator but is used in conjunction with the player
validMovesGenPlayer :: Board -> Player -> [ ((Int, Int), (Int, Int))] -> [ ((Int, Int) , (Int , Int))]

validMovesGenPlayer theBoard player [] = []
validMovesGenPlayer theBoard player ((source, dest) :xs) = if isValidForPlayer theBoard player ([(source)] ++ [(dest)])
														   then [(source, dest)] ++ validMovesGenPlayer theBoard player xs
														   else validMovesGenPlayer theBoard player xs
--Additional check to ensure a player can only move their own piece
isValidForPlayer :: Board -> Player -> [(Int,Int)] -> Bool
isValidForPlayer board player [] = False
isValidForPlayer board player (first:rest) =  if (((getFromBoard board first) == E) || (player == playerOf (pieceOf(getFromBoard board first))))  
                                              then isValidMove board first (head rest)
                                              else False
                                        
                                              

-- | Checks if a move is valid or not on the game board 
isValidMove :: Board -> (Int,Int) -> (Int,Int) -> Bool
isValidMove theBoard (x,y) (w,z)
    | (x<0) || (x>4)|| (y<0) || (y>4) || (w<0) || (w>4) || (z<0) || (z>4) = False
    |((getFromBoard theBoard (x,y)) == WK ) =               -- | A knights move is valid if it:
        if (((abs (x-w))<3) && ((abs (y-z)))<3)             -- | moves 2 spaces on one axis, and one space on the other 
           && (((abs (x-w)) + (abs (y-z))) == 3)            -- | (totaling 3 spaces)
           && (((getFromBoard theBoard (w,z)) ==  E)        -- | and if the target space is empty 
               || ((getFromBoard theBoard (w,z)) ==  BK)    -- |or contains an opponent's piece.
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

    |((getFromBoard theBoard (x,y)) == WP ) =               -- | A pawns move is valid if it:
        if (((abs (x-w)) == 0) && ((y-z) == -1) && getFromBoard theBoard (w,z) == E)            -- | moves one space vertically into an open space
        then True                                           -- | or move diagonally one space onto a space 
        else if (((abs (x-w)) == 1) && ((y-z) == -1))       -- | occupied by an opponent's piece
             && ( ((getFromBoard theBoard (w,z)) ==  BK)
               || ((getFromBoard theBoard (w,z)) ==  BP))
             then True
             else False

    |((getFromBoard theBoard (x,y)) == BP ) =
        if (((abs (x-w)) == 0) && ((y-z) == 1) && getFromBoard theBoard (w,z)== E)
        then True
        else if (((abs (x-w)) == 1) && ((y-z) == 1)) 
             && ( ((getFromBoard theBoard (w,z)) ==  WK)
               || ((getFromBoard theBoard (w,z)) ==  WP))
             then True
             else False

    |((getFromBoard theBoard (x,y)) == E )  = False

formatStringForGreedy :: String -> String 
formatStringForGreedy s = filter helperFormatStringForGreedy (removeFront s)

-- Removes " _ _ _ _ _\n" from the String
removeFront :: String -> String 

removeFront (x:xs) | x == '\n'  && length xs  >= 0= [] ++ xs
removeFront (x:xs) = removeFront xs 

helperFormatStringForGreedy :: Char -> Bool
helperFormatStringForGreedy '|' = False
helperFormatStringForGreedy '\n' = False
helperFormatStringForGreedy  c = True

generateMovesForGreedyStrat :: Board -> [ ((Int, Int), (Int, Int))] 

generateMovesForGreedyStrat b = generateMovesForGreedyStratString (formatStringForGreedy (board2Str b)) 0

generateMovesForGreedyStratString :: String -> Int  -> [((Int, Int) , (Int, Int))]
generateMovesForGreedyStratString [] _ = []
generateMovesForGreedyStratString (c: cs) prevLen |  (prevLen + length(c:cs)) > 29 || (prevLen + length(c:cs)) < 0 = []
generateMovesForGreedyStratString (c: cs) prevLen |  (prevLen) > 19 = 
                              generateMovesForGreedyStratUsingChar c (4, (prevLen) `mod` 5) ++ generateMovesForGreedyStratString cs (prevLen +1)
generateMovesForGreedyStratString (c: cs) prevLen |  (prevLen) > 14 = 
                              generateMovesForGreedyStratUsingChar c (3, (prevLen) `mod` 5) ++ generateMovesForGreedyStratString cs (prevLen +1)
generateMovesForGreedyStratString (c: cs) prevLen |  (prevLen) > 9 = 
                              generateMovesForGreedyStratUsingChar c (2, (prevLen) `mod` 5) ++ generateMovesForGreedyStratString cs (prevLen +1)
generateMovesForGreedyStratString (c: cs) prevLen |  (prevLen) > 4 = 
                              generateMovesForGreedyStratUsingChar c (1, (prevLen) `mod` 5) ++ generateMovesForGreedyStratString cs (prevLen +1)
generateMovesForGreedyStratString (c: cs) prevLen |  (prevLen) >= 0 = 
                              generateMovesForGreedyStratUsingChar c (0, (prevLen `mod` 5)) ++ generateMovesForGreedyStratString cs (prevLen +1)
                 
generateMovesForGreedyStratUsingChar :: Char -> (Int, Int) -> [ ((Int, Int), (Int, Int))]

generateMovesForGreedyStratUsingChar c (x,y) | c == '_' = []
generateMovesForGreedyStratUsingChar c (x,y) | c == '/' || c == '+' = [ ((x,y), (x, y+1))]++ [ ((x,y), (x -1 , y))] ++ [((x,y), (x+1, y+1))] ++ [((x,y) , (x -1, y +1))] ++
                                            [((x,y) , (x, y -1 ))] ++ [((x,y), (x - 1, y))] ++ [((x,y) , (x-1, y -1))] ++ [((x,y), (x +1 , y -1))]
generateMovesForGreedyStratUsingChar c (x,y) | c == 'X' || c == '#' = [ ((x,y) , (x - 2, y + 1))] ++ [((x,y) , (x-1, y+2))] ++ [((x,y) , (x+1, y+2))] ++ [((x, y) , (x+2, y+1))] ++
                                                                      [ ((x,y) , (x + 2, y - 1))] ++ [((x,y) , (x+1, y-2))] ++ [((x,y) , (x-1, y-2))] ++ [((x, y) , (x-2, y-1))] 

    
                                        
