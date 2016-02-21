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
                          
greedyTestA :: GameState
greedyTestA = GameState Init 0 Init 0
                        [[WK,WP,WP,WP,WK],
                         [WP,E,E,E,E],
                         [E,E,E,E,WP],
                         [BP,E,BK,E,BP],
                         [BK,BP,BP,BP,E]]





--EndTestCode


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:
     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}

-- | This is main', this will get player strategies by checking the cmd line args, or by calling getGameMode, then it will call gameLoop

main'           :: [String] -- ^ command line arguments
                -> IO() 
main' args | args == [] =     do    
                                choices <- getGameMode
                                if(checkStrings choices == True)
                                then gameLoop initBoard (head choices) (last choices)
                                else putStrLn "Invalid strategy selected. Available strategies are:\n  human\n  greedy\n random"
main' args | length args == 2 = if(checkStrings args == True)
                                then gameLoop initBoard (head args) (last args)
                                else putStrLn "Invalid strategy selected. Available strategies are:\n  human\n  greedy\n random"


-- | The checkStrings function will check to see if each element in a list of strings is a valid player strategy
checkStrings :: [String] -- ^ list of player strategies
                 -> Bool -- ^ True if valid, False if not valid
                 
checkStrings [] = False --empty list always returns false
checkStrings [singleElement] = False -- false if list has only one element
checkStrings (black:white:other) | (black /= "human" && black /= "greedy" && black /= "random") = False -- false if =/ "human", "greedy" or "random"
                                 | (white /= "human" && white /= "greedy" && white /= "random") = False
                                 | True = True -- true if none of the above conditionals were satisfied
 
 
   
-- If there is a winner on the board, or if both players have passed there turns. 
-- If they have, do not continue with another turn

-- | The gameLoop function is the overall game being played in a recursive loop
gameLoop :: GameState -- ^ state of game
         -> String -- ^ white player strategy
         -> String -- ^ black player strategy
         -> IO ()
gameLoop state wStrat bStrat | (blackPlay state == Passed) && (whitePlay state == Passed) 
                                || not(isWinner state == Nothing) =  do 
																		print state 
								
																		(putStrLn (getWinnerString state wStrat bStrat))
                             | ((pawn2Upgrade state) == True) && 
                               (((countPiece (flatten (theBoard state)) BK)  < 2) && (getPawnPlayer (theBoard state) (getPawn(theBoard state)) == Black))
                                = do 
                                    print state
                                                                                        
                                    (gameLoop (handlePromotion state (getPawnPlayer (theBoard state) (getPawn(theBoard state)))) wStrat bStrat)
                             | ((pawn2Upgrade state) == True) && 
                               (((countPiece (flatten (theBoard state)) WK)  < 2) && (getPawnPlayer (theBoard state) (getPawn(theBoard state)) == White))
                                = do 
                                    print state
                                                                                        
                                    (gameLoop (handlePromotion state (getPawnPlayer (theBoard state) (getPawn(theBoard state)))) wStrat bStrat)
                             | ((pawn2Upgrade state) == True) && (getPawnPlayer (theBoard state) (getPawn(theBoard state)) == Black)
                                       = do 
                                                print state
                                                putStr(if(bStrat == "human")
                                                then "Enter the coordinates to place the pawn for player Black in the form 'destX destY'\n(0 >= n >= 4, or just enter return for a 'pass') B2:\n"
                                                else "")
                                                move <- pickMove bStrat state PawnPlacement Black
                                                (gameLoop (handlePlacement state move Black) wStrat bStrat)
                             | ((pawn2Upgrade state) == True) && (getPawnPlayer (theBoard state) (getPawn(theBoard state)) == White)
                                       = do 
                                                print state
                                                putStr(if(wStrat == "human")
                                                then "Enter the coordinates to place the pawn for player White in the form 'destX destY'\n(0 >= n >= 4, or just enter return for a 'pass') W2:\n"
                                                else "")
                                                move <- pickMove wStrat state PawnPlacement White
                                                (gameLoop (handlePlacement state move White) wStrat bStrat)
                             | True = do  
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


-- | updateState is a function that will update the gameState depending on the given player moves in the parameters
updateState :: GameState -- ^ initial gameState
            -> Maybe ([(Int,Int)]) -- ^ black player move
            -> Maybe ([(Int,Int)]) -- ^ white player move
            -> GameState -- ^ return new gameState depending on the moves above
            
updateState state Nothing Nothing = GameState Passed (blackPen state) Passed (whitePen state) (theBoard state)
updateState state bMove wMove | (bMove == Nothing) && not(isValidForPlayer (theBoard state) White (fromJust wMove)) = 
										GameState Passed (blackPen state) (moveType state (fromJust wMove) White) (whitePen state +1) (theBoard state)
                              | (wMove == Nothing) && not(isValidForPlayer (theBoard state) Black (fromJust bMove)) = 
										GameState (moveType state (fromJust bMove) Black) (blackPen state+1) Passed (whitePen state) (theBoard state)
                              | (bMove == Nothing) || (wMove == Nothing) = if (bMove == Nothing)
                                      then GameState Passed (blackPen state) (moveType state (fromJust wMove) White) (whitePen state) 
                                      (handlePlayerMove (theBoard state) (fromJust wMove) White)
                                      else GameState (moveType state (fromJust bMove) Black) (blackPen state) Passed (whitePen state) 
                                      (handlePlayerMove (theBoard state) (fromJust bMove) Black)
                              | not(    isValidForPlayer (theBoard state) Black (fromJust bMove))
                                && not( isValidForPlayer (theBoard state) White (fromJust wMove))    
                                =  GameState (Goofed (format2Moves (fromJust bMove))) ((blackPen state)+1) (Goofed ( format2Moves (fromJust wMove) )) ((whitePen state)+1) (theBoard state)
                                                                       
                              | not(    isValidForPlayer (theBoard state) Black (fromJust bMove))
                                || not( isValidForPlayer (theBoard state) White (fromJust wMove))    
                                =  if    (isValidForPlayer (theBoard state) Black (fromJust bMove))
                                   then GameState (moveType state (fromJust bMove) Black) (blackPen state) (Goofed ( format2Moves (fromJust wMove) )) ((whitePen state)+1) 
                                   (handlePlayerMove (theBoard state) (fromJust bMove) Black)
                                       
                                   else GameState (Goofed (format2Moves (fromJust bMove))) ((blackPen state)+1) (moveType state (fromJust wMove) White) (whitePen state) 
                                   (handlePlayerMove (theBoard state) (fromJust wMove) White)
                                                                 
                              | isValidForPlayer (theBoard state) Black (fromJust bMove)
                                && isValidForPlayer (theBoard state) White (fromJust wMove)
                                         = GameState  (Played (head (fromJust bMove), head(tail (fromJust bMove))))
                                           (blackPen state)
                                           (Played (head (fromJust wMove), head(tail (fromJust wMove))))
                                           (whitePen state)
                                           (if not(isClash (head(tail (fromJust bMove))) (head(tail (fromJust wMove))))
                                           then (handleBothPlayerMoves (theBoard state) Black (fromJust bMove) White (fromJust wMove))
                                            else (handleClash (fromJust bMove) (fromJust wMove) (theBoard state)))

-- | format2Moves is a function that will reformat a list of moves, into 2 tuples, the first being the coordinates of the initial place, and second being final place.                                          
format2Moves :: [(Int,Int)] -- ^ given move within a list
             -> ((Int, Int), (Int, Int)) -- ^ return new move in 2 tuples, rather than a list

format2Moves move = (head move , head(tail move))


-- | This function will take a player move and determine if it was played, goofed, or if it was an upgrade from a pawn to a knight.
moveType :: GameState -- ^ Initial Game State
         -> ([(Int, Int)]) -- ^ Given Player Move
         -> Player -- ^ Player
         -> Played -- ^ Type Of Move

moveType state list p | length list == 1 = UpgradedPawn2Knight (head list)
moveType state ((x_s, y_s) : (x_d, y_d) : list) p | not(isValidMove (theBoard state) (x_s, y_s) (x_d, y_d))  = Goofed ((x_s, y_s), (x_d, y_d))
moveType state list p = Played ( (head list), list !! 1)


-- | handlePlayerMove is a function that will take a board, a move and a player and update the board with the given move
handlePlayerMove :: Board -- ^ Initial Game Board
                    -> ([(Int, Int)]) -- ^ Player Move
                    -> Player -- ^ Player
                    -> Board -- ^ return updated board

handlePlayerMove b ((x_s, y_s) : (x_d, y_d) : xs) p = 
                    replace2 (replace2 b (x_d, y_d) (getFromBoard b (x_s, y_s))) (x_s, y_s) E


-- | handleBothPlayerMoves is a function that will take the board, and 2 players along with there moves and update the board with those given moves.
handleBothPlayerMoves :: Board -- ^ Initial Game Board
                         -> Player -- ^ Black Player
                         -> ([(Int, Int)]) -- ^ Black Player Move
                         -> Player -- ^ White Player
                         -> ([(Int, Int)]) -- ^ White Player Move
                         -> Board -- ^ updated Game Board
                         
handleBothPlayerMoves board black bMove white wMove | ((head wMove) == (last bMove)) && ((head bMove) == (last wMove))
                                                      = handlePieceSwap board (getFromBoard board (head bMove)) (getFromBoard board (head wMove)) bMove
                                                    | (head wMove) == (last bMove) =  handlePlayerMove (handlePlayerMove board wMove white) bMove black
                                                    | (head bMove) == (last wMove) =  handlePlayerMove (handlePlayerMove board bMove black) wMove white
                                                    | True =  handlePlayerMove (handlePlayerMove board bMove black) wMove white


-- | handlePieceSwap is a function that swap 2 pieces on a board and update that given board
handlePieceSwap :: Board -- ^ Initial Game Board
                   -> Cell -- ^ Black Piece
                   -> Cell -- ^ White Piece
                   -> [(Int, Int)] -- ^ Player Move
                   -> Board -- ^ updated Game Board

handlePieceSwap board bPiece wPiece (black:white:rest) = replace2 (replace2 board white bPiece) black wPiece




-- | The pickMove function will take a string from the gameLoop, what kind of move it makes, and picks the corresponding strategy and returns the move
pickMove :: String -- ^ strategy
            -> GameState -- ^ Initial Game State
            -> PlayType -- ^ Kind of move
            -> Player -- ^ Player
            -> IO (Maybe[(Int,Int)]) -- ^ Move
pickMove strat state playtype player | strat == "human" = human state playtype player 
                                     | strat == "greedy" = greedy state playtype player 
                                     | strat == "random" = randomStrategy state playtype player
                                     | True = return Nothing
									 
{- |									 
The isWinner function will take the gameState, and determine of a player has won the game, if he has, it will return that player.
									 
	Game over conditions:
	-	One of the players looses all his/her pawns.  The other player is the winner. 
	-	One of the players accumulates two penalty points.  The other player is the winner.
	-	Both players pass on the same round. The one with the most pawns wins.
	-	This method will return the winner or return Nothing.
	-	Note it is not compatible with testing whether or not both players pass on the same round
-}
isWinner :: GameState -- ^ Initial Game State
            -> Maybe Player -- ^ Player who won (or nothing if nobody has won)

isWinner game | (blackPen game) >= 2 = Just(White)            --Black has 2 or more penalties which makes White the winner
isWinner game | (whitePen game) >= 2 = Just(Black)            --White has 2 or more penalties which makes White the winner

isWinner game = if    not (elem '/' (board2Str (theBoard game))) 
                   || not (elem '+' (board2Str (theBoard game)))
                then (if elem '/' (board2Str (theBoard game))
                      then Just(White)
                      else Just(Black))
                else Nothing
 
 

-- | getWinnerString is a function that will return a string that prints who won the game, given the gameState, and the player strategies               
getWinnerString :: GameState -- ^ Initial Game State
                   -> String -- ^ White Player Strategy
                   -> String -- ^ Black Player Strategy
                   -> String -- ^ Returned Information String
                   
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




-- | the isClash function will check to see if there is a clash on the game board (when destination of both moves is the same)              
isClash :: (Int,Int) -- ^ First Player Move
        -> (Int,Int) -- ^ Second Player Move
        -> Bool -- ^ True if both moves have same destination, otherwise False
isClash (x,y) (w,z) = if ((x == w)&&(y == z))
                      then True
                      else False

--Note: This function performs no error checking (assumes that the moves were valid and that there is a clash)

{- | handleClash is a function that will determine the outcome of a clash on the game board.

Handles the 2 possible clash scenarions
	-	Knight and Pawn on the same space = Knight destroys Pawn
	-	2 pieces of the same type on the same space = both pieces destroyed
	-	Returns the board in the new configuration when done
-}
handleClash :: [(Int, Int)] -- ^ Black Player Move
            -> [(Int, Int)] -- ^ White Player Move
            -> Board -- ^ Initial Game Board
            -> Board -- ^ Return new Game Board after the clash
handleClash (b1:bs) (w1:ws) board | ((getFromBoard board b1) == BK && (getFromBoard board w1) == WP) = 
                                    replace2 ( replace2 ( replace2 board (head bs) BK) b1 E) w1 E
                                  | ((getFromBoard board b1) == BP && (getFromBoard board w1) == WK) = 
                                    replace2 ( replace2 ( replace2 board (head ws) WK) b1 E) w1 E 
                                  | True =    replace2 ( replace2 board b1 E) w1 E



-- | pawn2Upgrade is a function that will check if there is a pawn to be promoted on either side of the board
pawn2Upgrade :: GameState -- ^ Initial gameState
                -> Bool -- Return True if there is a pawn to be upgraded, otherwise False.
pawn2Upgrade state = if (elem BP (head (theBoard state))) || (elem WP (last (theBoard state)))
                      then True
                      else False


-- | getPawn is a function that takes a Board, and returns a type with the location of the first pawn to be promoted (prioritizes Black pawns)
getPawn :: Board -- ^ Initial Game Board
           -> (Int,Int) -- ^ Coordinates of first pawn to be promoted
getPawn board = if(elem BP (head board))
        then (findPawn BP (head board), 0)
        else (findPawn WP (last board), 4)


-- | getPawnPlayer is a function that returns which player a certain pawn on the board belongs to.
getPawnPlayer :: Board -- ^ Initial Game Board
                 -> (Int,Int) -- ^ Coordinates of Pawn
                 -> Player -- ^ Return player who owns the pawn  in those coordinates
getPawnPlayer board coord = playerOf (pieceOf (getFromBoard board coord))


-- | The findPawn function will return the index in a list of the first instance of a pawn piece.   
findPawn :: Cell -- ^ Given piece
            -> [Cell] -- ^ List on the board
            -> Int -- ^ Return index of firstPawn
findPawn _ [] = -5
findPawn piece (x:xs) =  if (x == piece)
                         then 0
                         else 1 + findPawn piece xs
 
-- | flatten is a function that will take a 2d list and flatten it into a 1d list                                    
flatten :: [[a]] -- ^ Given 2d list
        -> [a] -- ^ Return 1d list
        
flatten [] = []
flatten (x:xs) = x ++ (flatten xs)


-- | countPiece is a function that counts the number of pieces in a list that are a specific kind of piece. 
countPiece :: [Cell] -- ^ List to search through
           -> Cell -- ^ type of piece
           -> Int -- ^ Return number of those pieces on the board
countPiece [] target = 0
countPiece (x:xs) target | x == target = 1 + (countPiece xs target)
                         | True = countPiece xs target
               
               
  
-- | handlePromotion is a function that will promote a pawn piece into a knight piece.                                   
handlePromotion :: GameState -- ^ Initial GameState
                -> Player  -- ^ Player
                -> GameState -- ^ Return new updated GameState with the upgraded pawn
                
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


-- | handlePlacement is a function that will take the gameState, a player move, a player and return a new gameState after that move.
handlePlacement :: GameState -- ^ Initial GameState
                -> Maybe[(Int,Int)] -- ^ Player Move
                -> Player -- ^ Player
                -> GameState -- ^ Return updated GameState
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
                                              
-- | Additional check to ensure a player can only move their own piece
isValidForPlayer :: Board -> Player -> [(Int,Int)] -> Bool
isValidForPlayer board player [] = False
isValidForPlayer board player (first:rest) | (getFromBoard board first) == E = False
										   | (player == playerOf (pieceOf(getFromBoard board first)))  = isValidMove board first (head rest)
                                           | True = False
                                      
{- |
	Checks if a move is valid or not on the game board 
	A knights move is valid if it:
		-	moves 2 spaces on one axis, and one space on the other (totaling 3 spaces)
		-	if the target space is empty or contains an opponent's piece.
	A pawns move is valid if it:
		-	moves one space vertically into an open space
		-	moves diagonally one space onto a space occupied by an opponent's piece
-}

isValidMove :: Board -> (Int,Int) -> (Int,Int) -> Bool
isValidMove theBoard (x,y) (w,z)
    | (x<0) || (x>4)|| (y<0) || (y>4) || (w<0) || (w>4) || (z<0) || (z>4) = False
    |((getFromBoard theBoard (x,y)) == WK ) =               
        if (((abs (x-w))<3) && ((abs (y-z)))<3)             
           && (((abs (x-w)) + (abs (y-z))) == 3)            
           && (((getFromBoard theBoard (w,z)) ==  E)       
               || ((getFromBoard theBoard (w,z)) ==  BK)    
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