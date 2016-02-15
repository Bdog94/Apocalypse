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
import Data.Char
import ApocStrategyGreedy
import ApocStrategyHuman
import ApocStrategyRandom

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
promoBoard2      = GameState Init 0 Init 0
                          [ [WK, WP, E, E, WK],
                          [E, E , E , E , E],
                          [E ,WP , E , E , E ],
                          [E, E , BP , E , E],
                          [BK, E, E,  WP , BK] ]
                          
promoBoard1       :: GameState
promoBoard1 = GameState Init 0 Init 0
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
main' args | args == [] =     do    
                                choices <- getGameMode
                                if(checkStrings choices == True)
                                then gameLoop initBoard (head choices) (last choices)
                                else putStrLn "Invalid strategy selected. Available strategies are:\n  human\n  greedy"
main' args | length args == 2 = if(checkStrings args == True)
                                then gameLoop initBoard (head args) (last args)
                                else putStrLn "Invalid strategy selected. Available strategies are:\n  human\n  greedy"

checkStrings :: [String] -> Bool
checkStrings [] = False
checkStrings (black:white:other) | (black /= "human" && black /= "greedy") = False 
                                 | (white /= "human" && white /= "greedy") = False
                                 | True = True
   
-- If there is a winner on the board, or if both players have passed there turns. 
-- If they have, do not continue with another turn
gameLoop :: GameState -> String -> String -> IO ()
gameLoop state wStrat bStrat | (blackPlay state == Passed) && (whitePlay state == Passed) 
                                || not(isWinner state == Nothing) =  do 
																		print state 
								
																		(putStrLn (getWinnerString state wStrat bStrat))
gameLoop state wStrat bStrat | ((pawn2Upgrade state) == True) && 
                               (((countPiece (flatten (theBoard state)) BK) 
                                + (countPiece (flatten (theBoard state)) WK) )< 4) = 
                                do 
                                    print state
                                                                                        
                                    (gameLoop (handlePromotion state (getPawnPlayer (theBoard state) (getPawn(theBoard state)))) wStrat bStrat)
gameLoop state wStrat bStrat | ((pawn2Upgrade state) == True) && (getPawnPlayer (theBoard state) (getPawn(theBoard state)) == Black)
                                       = do 
                                                print state
                                                putStr(if(bStrat == "human")
                                                then "Enter the coordinates to place the pawn for player Black in the form 'destX destY'\n(0 >= n >= 4, or just enter return for a 'pass') B2:\n"
                                                else "")
                                                move <- pickMove bStrat state PawnPlacement Black
                                                (gameLoop (handlePlacement state move Black) wStrat bStrat)
gameLoop state wStrat bStrat | ((pawn2Upgrade state) == True) && (getPawnPlayer (theBoard state) (getPawn(theBoard state)) == White)
                                       = do 
                                                print state
                                                putStr(if(wStrat == "human")
                                                then "Enter the coordinates to place the pawn for player White in the form 'destX destY'\n(0 >= n >= 4, or just enter return for a 'pass') W2:\n"
                                                else "")
                                                move <- pickMove wStrat state PawnPlacement White
                                                (gameLoop (handlePlacement state move White) wStrat bStrat)
gameLoop state wStrat bStrat | True = do  
                                            print state
                                        
                                            putStr (if(bStrat == "human")
                                            then "Enter the move coordinates for player Black in the form 'srcX srcY destX destY'\n(0 >= n >= 4, or just enter return for a 'pass') B1:\n"
                                            else "")
                                            bMove <- pickMove bStrat state Normal Black    
                                            putStr (if(wStrat == "human")
                                            then "Enter the move coordinates for player White in the form 'srcX srcY destX destY'\n(0 >= n >= 4, or just enter return for a 'pass') W1:\n"
                                            else "")
                                            wMove <- pickMove wStrat state Normal White
                                            gameLoop (updateState state bMove wMove) wStrat bStrat
                                        
                                        
-- Takes the current GameState and the 2 players moves
-- Returns an updated GameState with moves performed

--Only works with Normal moves, doesn't account for pawn placement or goofs by the player

--TODO
--Pawn promotion
--Handle isValid
--Only works with Normal moves, doesn't account for pawn placement or goofs by the player

updateState :: GameState -> Maybe ([(Int,Int)]) -> Maybe ([(Int,Int)]) -> GameState
updateState state Nothing Nothing = GameState Passed (blackPen state) Passed (whitePen state) (theBoard state)
updateState state bMove wMove | (bMove == Nothing) && not(isValidForPlayer (theBoard state) White (fromJust wMove)) = 
										GameState Passed (blackPen state) (moveType state (fromJust wMove) White) (whitePen state +1) (theBoard state)
updateState state bMove wMove | (wMove == Nothing) && not(isValidForPlayer (theBoard state) Black (fromJust bMove)) = 
										GameState (moveType state (fromJust bMove) Black) (blackPen state+1) Passed (whitePen state) (theBoard state)
updateState state bMove wMove | (bMove == Nothing) || (wMove == Nothing) = if (bMove == Nothing)
                                      then GameState Passed (blackPen state) (moveType state (fromJust wMove) White) (whitePen state) 
                                      (handlePlayerMove (theBoard state) (fromJust wMove) White)
                                      else GameState (moveType state (fromJust bMove) Black) (blackPen state) Passed (whitePen state) 
                                      (handlePlayerMove (theBoard state) (fromJust bMove) Black)
updateState state bMove wMove | not(    isValidForPlayer (theBoard state) Black (fromJust bMove))
                                && not( isValidForPlayer (theBoard state) White (fromJust wMove))    
                                =  GameState (Goofed (format2Moves (fromJust bMove))) ((blackPen state)+1) (Goofed ( format2Moves (fromJust wMove) )) ((whitePen state)+1) (theBoard state)
                                                                       
updateState state bMove wMove | not(    isValidForPlayer (theBoard state) Black (fromJust bMove))
                                || not( isValidForPlayer (theBoard state) White (fromJust wMove))    
                                =  if    (isValidForPlayer (theBoard state) Black (fromJust bMove))
                                   then GameState (moveType state (fromJust bMove) Black) (blackPen state) (Goofed ( format2Moves (fromJust wMove) )) ((whitePen state)+1) 
                                   (handlePlayerMove (theBoard state) (fromJust bMove) Black)
                                       
                                   else GameState (Goofed (format2Moves (fromJust bMove))) ((blackPen state)+1) (moveType state (fromJust wMove) White) (whitePen state) 
                                   (handlePlayerMove (theBoard state) (fromJust wMove) White)
                                                                 
updateState state bMove wMove | isValidForPlayer (theBoard state) Black (fromJust bMove)
                                && isValidForPlayer (theBoard state) White (fromJust wMove)
                                         = GameState  (Played (head (fromJust bMove), head(tail (fromJust bMove))))
                                           (blackPen state)
                                           (Played (head (fromJust wMove), head(tail (fromJust wMove))))
                                           (whitePen state)
                                           (if not(isClash (head(tail (fromJust bMove))) (head(tail (fromJust wMove))))
                                           then (handleBothPlayerMoves (theBoard state) Black (fromJust bMove) White (fromJust wMove))
                                            else (handleClash (fromJust bMove) (fromJust wMove) (theBoard state)))
                                            
format2Moves :: [(Int,Int)] -> ((Int, Int), (Int, Int))
format2Moves move = (head move , head(tail move))

moveType :: GameState -> ([(Int, Int)]) -> Player -> Played

moveType state list p | length list == 1 = UpgradedPawn2Knight (head list)
moveType state ((x_s, y_s) : (x_d, y_d) : list) p | not(isValidMove (theBoard state) (x_s, y_s) (x_d, y_d))  = Goofed ((x_s, y_s), (x_d, y_d))
moveType state list p = Played ( (head list), list !! 1)

handlePlayerMove :: Board -> ([(Int, Int)]) -> Player -> Board

handlePlayerMove b ((x_s, y_s) : (x_d, y_d) : xs) p = 
                    replace2 (replace2 b (x_d, y_d) (getFromBoard b (x_s, y_s))) (x_s, y_s) E

handleBothPlayerMoves :: Board -> Player -> ([(Int, Int)]) -> Player -> ([(Int, Int)]) -> Board
handleBothPlayerMoves board black bMove white wMove | (head wMove) == (last bMove) =  handlePlayerMove (handlePlayerMove board wMove white) bMove black
handleBothPlayerMoves board black bMove white wMove | (head bMove) == (last wMove) =  handlePlayerMove (handlePlayerMove board bMove black) wMove white
handleBothPlayerMoves board black bMove white wMove | True =  handlePlayerMove (handlePlayerMove board bMove black) wMove white


--Takes a string from gameLoop, what kind of move to make, and picks the corresponding strategy and returns it's move 
pickMove :: String -> GameState -> PlayType -> Player -> IO (Maybe[(Int,Int)])
pickMove strat state playtype player | strat == "human" = human state playtype player 
pickMove strat state playtype player | strat == "greedy" = greedy state playtype player 
pickMove strat state playtype player | True = return Nothing


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
                
getWinnerString :: GameState -> String -> String -> String
getWinnerString state wStrat bStrat| (whitePen state) >= 2 =    "Black wins!\tBlack (" ++ bStrat ++ "): " ++ [(intToDigit(countPiece (flatten (theBoard state)) BP ))]
        ++ "\tWhite (" ++ wStrat ++ "): " ++ [(intToDigit(countPiece (flatten (theBoard state)) WP))]
getWinnerString state wStrat bStrat| (blackPen state) >= 2 =    "White wins!\tBlack (" ++ bStrat ++ "): " ++ [(intToDigit(countPiece (flatten (theBoard state)) BP ))]
        ++ "\tWhite (" ++ wStrat ++ "): " ++ [(intToDigit(countPiece (flatten (theBoard state)) WP))]    
getWinnerString state wStrat bStrat| countPiece (flatten (theBoard state)) BP == countPiece (flatten (theBoard state)) WP 
    =    "The game was a Draw."
getWinnerString state wStrat bStrat| countPiece (flatten (theBoard state)) BP > countPiece (flatten (theBoard state)) WP 
    =    "Black wins!\tBlack (" ++ bStrat ++ "): " ++ [(intToDigit(countPiece (flatten (theBoard state)) BP ))]
        ++ "\tWhite (" ++ wStrat ++ "): " ++ [(intToDigit(countPiece (flatten (theBoard state)) WP))]
getWinnerString state wStrat bStrat| countPiece (flatten (theBoard state)) BP < countPiece (flatten (theBoard state)) WP 
    =    "White wins!\tBlack (" ++ bStrat ++ "): " ++ [(intToDigit(countPiece (flatten (theBoard state)) BP ))]
        ++ "\tWhite (" ++ wStrat ++ "): " ++ [(intToDigit(countPiece (flatten (theBoard state)) WP))]    


--Checks if there is a clash on the board (the destination of both moves is the same)                
isClash :: (Int,Int) -> (Int,Int) -> Bool
isClash (x,y) (w,z) = if ((x == w)&&(y == z))
                      then True
                      else False

--Handles the 2 possible clash scenarions
--Knight and Pawn on the same space = Knight destroys Pawn
--2 pieces of the same type on the same space = both pieces destroyed
--Returns the board in the new configuration when done

--Note: This function performs no error checking (assumes that the moves were valid and that there is a clash)
handleClash :: [(Int, Int)] -> [(Int, Int)] -> Board -> Board
handleClash (b1:bs) (w1:ws) board | ((getFromBoard board b1) == BK && (getFromBoard board w1) == WP) = 
                                    replace2 ( replace2 ( replace2 board (head bs) BK) b1 E) w1 E
                                  | ((getFromBoard board b1) == BP && (getFromBoard board w1) == WK) = 
                                    replace2 ( replace2 ( replace2 board (head ws) WK) b1 E) w1 E 
                                  | True =    replace2 ( replace2 board b1 E) w1 E

-- Checks if there is a pawn to be promoted on either side of the board
pawn2Upgrade :: GameState -> Bool
pawn2Upgrade state = if (elem BP (head (theBoard state))) || (elem WP (last (theBoard state)))
                      then True
                      else False

--Takes a Board, returns a tuple with the location of the first pawn to be promoted (prioritizes Black pawns)
getPawn :: Board -> (Int,Int)
getPawn board = if(elem BP (head board))
        then (findPawn BP (head board), 0)
        else (findPawn WP (last board), 4)

getPawnPlayer :: Board -> (Int,Int) -> Player
getPawnPlayer board coord = playerOf (pieceOf (getFromBoard board coord))
--Returns the index in a list of the first instance of a piece        
findPawn :: Cell -> [Cell] -> Int
findPawn _ [] = -5
findPawn piece (x:xs) =  if (x == piece)
                         then 0
                         else 1 + findPawn piece xs
                                    
flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ (flatten xs)

countPiece :: [Cell] -> Cell -> Int
countPiece [] target = 0
countPiece (x:xs) target | x == target = 1 + (countPiece xs target)
                         | True = countPiece xs target
                                  
handlePromotion :: GameState -> Player -> GameState
handlePromotion state player | player == Black = GameState (UpgradedPawn2Knight(getPawn (theBoard state)))
                                                            (blackPen state)
                                                            None
                                                            (whitePen state)
                                                            (replace2 (theBoard state) (getPawn (theBoard state)) BK)
handlePromotion state player | player == White = GameState None
                                                            (blackPen state)
                                                            (UpgradedPawn2Knight(getPawn (theBoard state)))
                                                            (whitePen state)
                                                            (replace2 (theBoard state) (getPawn (theBoard state)) WK)

handlePlacement :: GameState -> Maybe[(Int,Int)] -> Player -> GameState
handlePlacement state move player | move == Nothing && player == Black =
                                   GameState NullPlacedPawn 
                                   (blackPen state) 
                                   None 
                                   (whitePen state) 
                                   (theBoard state) 

                                   | move == Nothing && player == White =
                                   GameState None
                                   (blackPen state) 
                                   NullPlacedPawn
                                   (whitePen state) 
                                   (theBoard state) 

                                   | player == Black && ((getFromBoard (theBoard state) (head (fromJust move))) == E) = 
                                   GameState (PlacedPawn((getPawn (theBoard state)),(head (fromJust move)))) 
                                   (blackPen state) 
                                   None 
                                   (whitePen state) 
                                   (handlePlayerMove (theBoard state) [getPawn(theBoard state),(head (fromJust move))] Black) 

                                   | player == Black = 
                                   GameState (BadPlacedPawn((getPawn (theBoard state)),(head (fromJust move)))) 
                                   (blackPen state + 1) 
                                   None 
                                   (whitePen state) 
                                   (theBoard state)

                                   | player == White && ((getFromBoard (theBoard state) (head (fromJust move))) == E)= 
                                   GameState None 
                                   (blackPen state) 
                                   (PlacedPawn((getPawn (theBoard state)),(head (fromJust move)))) 
                                   (whitePen state) 
                                   (handlePlayerMove (theBoard state) [getPawn(theBoard state),(head (fromJust move))] White) 

                                   | player == White = GameState None 
                                   (blackPen state) 
                                   (BadPlacedPawn((getPawn (theBoard state)),(head (fromJust move)))) 
                                   (whitePen state + 1) 
                                   (theBoard state)
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


-- | Similar to validMovesGenerator but is used in conjunction with the player
validMovesGenPlayer :: Board -> Player -> [ ((Int, Int), (Int, Int))] -> [ ((Int, Int) , (Int , Int))]

validMovesGenPlayer theBoard player [] = []
validMovesGenPlayer theBoard player ((source, dest) :xs) = if isValidForPlayer theBoard player ([(source)] ++ [(dest)])
														   then [(source, dest)] ++ validMovesGenPlayer theBoard player xs
														   else validMovesGenPlayer theBoard player xs


--  | This method will determine the list of valid moves
validMovesGenerator :: Board -> [  ((Int , Int) , (Int , Int))] -> [ ((Int , Int) , (Int , Int) )]
validMovesGenerator theBoard [] = []
validMovesGenerator theBoard ((x,y):xs) =  if isValidMove theBoard x y
                                       then [(x,y)] ++ validMovesGenerator theBoard xs
                                       else validMovesGenerator theBoard xs
                                       

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

generateMovesForGreedyStratString [] prevLen = []
generateMovesForGreedyStratString (c: cs) prevLen |  (prevLen + length(c:cs)) > 29 || (prevLen + length(c:cs)) < 4 = []
generateMovesForGreedyStratString (c: cs) prevLen |  (prevLen + length(c:cs)) > 4 = 
                 generateMovesForGreedyStratUsingChar c (0, (prevLen + length(c:cs)) `mod` 5) ++ generateMovesForGreedyStratString cs (prevLen +1)
generateMovesForGreedyStratString (c: cs) prevLen |  (prevLen + length(c:cs)) > 9 = 
                 generateMovesForGreedyStratUsingChar c (1, (prevLen + length(c:cs)) `mod` 5) ++ generateMovesForGreedyStratString cs (prevLen +1)
generateMovesForGreedyStratString (c: cs) prevLen |  (prevLen + length(c:cs)) > 14 = 
                 generateMovesForGreedyStratUsingChar c (2, (prevLen + length(c:cs)) `mod` 5) ++ generateMovesForGreedyStratString cs (prevLen +1)
generateMovesForGreedyStratString (c: cs) prevLen |  (prevLen + length(c:cs)) > 19 = 
                 generateMovesForGreedyStratUsingChar c (3, (prevLen + length(c:cs)) `mod` 5) ++ generateMovesForGreedyStratString cs (prevLen +1)
generateMovesForGreedyStratString (c: cs) prevLen |  (prevLen + length(c:cs)) > 24 = 
                 generateMovesForGreedyStratUsingChar c (4, (prevLen + length(c:cs)) `mod` 5) ++ generateMovesForGreedyStratString cs (prevLen +1)
                 
generateMovesForGreedyStratUsingChar :: Char -> (Int, Int) -> [ ((Int, Int), (Int, Int))]
generateMovesForGreedyStratUsingChar c (x,y) | c == 'E' = []
generateMovesForGreedyStratUsingChar c (x,y) | c == '/' || c == '+' = [ ((x,y), (x, y+1))]++ [ ((x,y), (x -1 , y))] ++ [((x,y), (x+1, y+1))] ++ [((x,y) , (x -1, y +1))] ++
                                            [((x,y) , (x, y -1 ))] ++ [((x,y), (x - 1, y))] ++ [((x,y) , (x-1, y -1))] ++ [((x,y), (x +1 , y -1))]





generateMovesForGreedyStrat' :: Board -> (Int, Int) -> [ ((Int, Int), (Int, Int))]

generateMovesForGreedyStrat' _ _ = []
--generateMovesForGreedyStrat' b (x,y) |  (x > 5 || y > 5) || (x < 0 || y < 0) = []
--generateMovesForGreedyStrat' b (x,y) |  (getFromBoard b (x,y)) == E = []
--generateMovesForGreedyStrat' b (x,y) |  (getFromBoard b (x,y)) == WP  || getFromBoard b (x,y) == BP
--                                       = [ ((x,y), (x, y+1)),  ((x,y) , (x +1, y)), ((x, y),(x+1,y+1)) , ((x,y), (x -1, y+1)) 
--                                              ((x,y), (x, y-1)),  ((x,y) , (x -1, y)), ((x, y),(x-1,y-1)) , ((x,y), (x +1, y -1))]
                                              
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
