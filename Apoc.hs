{- | This module is used for CPSC 449 for the Apocalypse assignment.

Feel free to modify this file as you see fit.

Copyright: Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose is hereby granted without fee, provided
that the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation. The University of Calgary makes no representations about the
suitability of this software for any purpose. It is provided "as is" without
express or implied warranty.

-}

module Main(main) where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.List
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:

     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args = do
    putStrLn "\nThe initial board:"
    print initBoard

    putStrLn $ "\nThe initial board with back human (the placeholder for human) strategy having played one move\n"
               ++ "(clearly illegal as we must play in rounds!):"
    move <- human (initBoard) Normal Black
    putStrLn (show $ GameState (if move==Nothing
                                then Passed
                                else Played (head (fromJust move), head (tail (fromJust move))))
                               (blackPen initBoard)
                               (Passed)
                               (whitePen initBoard)
                               (replace2 (replace2 (theBoard initBoard)
                                                   ((fromJust move) !! 1)
                                                   (getFromBoard (theBoard initBoard) ((fromJust move) !! 0)))
                                         ((fromJust move) !! 0)
                                         E))

---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)


-- | Checks if a move is valid or not on the game board 
isValidMove :: Board -> (Int,Int) -> (Int,Int) -> Bool
isValidMove theBoard (x,y) (w,z)
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
        if (((abs (x-w)) == 0) && ((y-z) == -1))            -- | moves one space vertically into an open space
        then True                                           -- | or move diagonally one space onto a space 
        else if (((abs (x-w)) == 1) && ((y-z) == -1))       -- | occupied by an opponent's piece
             && ( ((getFromBoard theBoard (w,z)) ==  BK)
               || ((getFromBoard theBoard (w,z)) ==  BP))
             then True
             else False

    |((getFromBoard theBoard (x,y)) == BP ) =
        if (((abs (x-w)) == 0) && ((y-z) == 1))
        then True
        else if (((abs (x-w)) == 1) && ((y-z) == 1)) 
             && ( ((getFromBoard theBoard (w,z)) ==  WK)
               || ((getFromBoard theBoard (w,z)) ==  WP))
             then True
             else False

    |((getFromBoard theBoard (x,y)) == E )  = False