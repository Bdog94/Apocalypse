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

--TestCode 
gameOverBoard1       :: GameState
gameOverBoard1      = GameState Init 0 Init 0
                          [ [WK, E, E, E, WK],
                        [E, E , E , E , E],
                        [E , E , E , E , E ],
                        [BP, E , E , E , BP],
                    [    BK, BP, BP, BP, BK] ]

gameOverBoard2       :: GameState
gameOverBoard2      = GameState Init 0 Init 0
                          [ [WK, WP, E, E, WK],
                          [E, E , E , E , E],
                          [E ,E , E , E , E ],
                          [E, E , E , E , E],
                          [BK, E, E,  E , BK] ]

testBlackWin         :: Bool

testBlackWin = if (isWinner gameOverBoard1 == Just(Black)) 
                then True
                else False
                
testWhiteWin         :: Bool

testWhiteWin = if (isWinner gameOverBoard2 == Just(White)) 
                then True
                else False


promoBoard2       :: GameState
promoBoard2      = GameState Passed 0 Passed 0
                          [ [WK, WP, E, E, WK],
                          [E, E , E , E , E],
                          [E ,WP , E , E , E ],
                          [E, E , BP , E , E],
                          [BK, E, E,  WP , BK] ]
                          
promoBoard1       :: GameState
promoBoard1 = GameState Passed 0 Passed 0
                          [ [WK, BP, E, E, WK],
                          [E, E , E , E , E],
                          [E ,WP , E , E , E ],
                          [E, E , BP , E , E],
                          [BK, E, E,  E , BK] ]



--EndTestCode


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

   
-- If there is a winner on the board, or if both players have passed there turns. 
-- If they have, do not continue with another turn
gameLoop :: GameState -> String -> String -> IO ()
gameLoop state wStrat bStrat =  if (blackPlay state == Passed) && (whitePlay state == Passed) || not(isWinner state == Nothing)
                                then do 
                                        print state--handle win conditions here`
                                else do 
                                        print state
                                
                                        bMove <- pickMove bStrat state Normal Black    
                                        wMove <- pickMove wStrat state Normal White
                                        --if(bMove == Nothing)||(wMove == Nothing)
                                        gameLoop state wStrat bStrat

										
-- Takes the current GameState and the 2 players moves
-- Returns an updated GameState with moves performed

--Only works with Normal moves, doesn't account for pawn placement or goofs by the player

updateState :: GameState -> Maybe ([(Int,Int)]) -> Maybe ([(Int,Int)]) -> GameState
updateState state bMove wMove = GameState (if bMove == Nothing
										   then Passed
										   else Played (head (fromJust bMove), head(tail (fromJust bMove))))
										   (blackPen state)
										   (if wMove == Nothing
										   then Passed
										   else Played (head (fromJust wMove), head(tail (fromJust wMove))))
										   (whitePen state)
										   (replace2 (
										   replace2 (
										   replace2 (
										   replace2 (theBoard state) ((fromJust bMove) !! 1)
                                                   (getFromBoard (theBoard state) ((fromJust bMove) !! 0))) 
												   ((fromJust bMove) !! 0) E)
												   ((fromJust wMove) !! 1) 
												   (getFromBoard (theBoard state) ((fromJust wMove) !! 0)))
												   ((fromJust wMove) !! 0) E)
--Takes a string from gameLoop, what kind of move to make, and picks the corresponding strategy and returns it's move 
pickMove :: String -> GameState -> PlayType -> Player -> IO (Maybe[(Int,Int)])
pickMove strat state playtype player = if strat == "human" 
                        then human state playtype player 
                        else return Nothing

-- Checks if there is a pawn to be promoted on either side of the board
pawnPromotion :: GameState -> Bool
pawnPromotion state = if (elem BP (head (theBoard state))) || (elem WP (last (theBoard state)))
                      then True
                      else False
--Takes a Board, returns a tuple with the location of the first pawn to be promoted (prioritizes Black pawns)
getPawn :: Board -> (Int,Int)
getPawn board = if(elem BP (head board))
		then (findPawn BP (head board), 0)
		else (findPawn WP (last board), 4)

--Returns the index in a list of the first instance of a piece		
findPawn :: Cell -> [Cell] -> Int
findPawn _ [] = -1
findPawn piece (x:xs) =  if (x == piece)
						 then 0
						 else 1 + findPawn piece xs
					  
-- Game over conditions
--One of the players looses all his/her pawns.  The other player is the winner. 
--One of the players accumulates two penalty points.  The other player is the winner.
--Both players pass on the same round. The one with the most pawns wins.


-- | This method will return the winner or return Nothing.
-- | Note it is not compatible with testing whether or not both players pass on the same round
-- that functionality is easier to do in the gameLoop I believe
isWinner :: GameState -> Maybe Player

isWinner game | (blackPen game) >= 2 = Just(White)            --Black has 2 or more penalties which makes White the winner
isWinner game | (whitePen game) >= 2 = Just(Black)            --White has 2 or more penalties which makes White the winner

isWinner game = if    not (elem '/' (board2Str (theBoard game))) 
                   || not (elem '+' (board2Str (theBoard game)))
                then (if elem '/' (board2Str (theBoard game))
                      then Just(White)
                      else Just(Black))
                else Nothing


isClash :: (Int,Int) -> (Int,Int) -> Bool

isClash (x,y) (w,z) = if ((x == w)&&(y == z))
                      then True
                      else False


                      
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


--  | This method will determine the list of valid moves
validMovesGenerator :: Board -> [  ((Int , Int) , (Int , Int))] -> [ ((Int , Int) , (Int , Int) )]
validMovesGenerator theBoard [] = []
validMovesGenerator theBoard ((x,y):xs) =  if isValidMove theBoard x y
                                       then [(x,y)] ++ validMovesGenerator theBoard xs
                                       else validMovesGenerator theBoard xs
                                       

-- | Generates all possible moves
--generateAllMoves :: () -> [ ((Int, Int), (Int, Int))]

-- Note this is code that once it is done will be used to generate every possible move
--generateAllMovesHelper :: Int -> [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
--generateAllMovesHelper 0 list = list
--generateAllMovesHelper 1 list = ((0, 0) , (0,0))
--generateAllMovesHelper size list = 
--generateAllMovesHelper size list  | size < 25 = [(5 - 25, 5)

                                      

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
