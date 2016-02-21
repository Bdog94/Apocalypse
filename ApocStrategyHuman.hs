module ApocStrategyHuman where



 --IMPORTS--
 
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import ApocTools
import Data.Char

-------------------------------------------------------------------------------------------------------------------------------------------------


human    :: Chooser
human b Normal        c = getMove
human b PawnPlacement c = getPlacement



-- | This is the getMove function, it will take a 4 digit move input and return it in a list, filtering out spaces in the process and ignoring a comment after.

getMove = do
    theMove <- getLine --set input to the input the user enters
    let input = filter(/=' ') theMove
    if input == [] then do return Nothing --return nothing if player passes
    else return (Just[(digitToInt(input!!0), digitToInt(input!!1)), (digitToInt(input!!2), digitToInt(input!!3))])-- return move while converting chars from input to ints
 
 
   
-- | This is the getPlacement function, it will take a 2 digit move (for pawn placement) and return it in an array filtering out spaces and comments. 

getPlacement = do
    theMove <- getLine --set input to the input the user enters
    let input = filter(/=' ') theMove -- filter out spaces
    if input == [] then do return Nothing -- return nothing if player passes
    else return (Just[(digitToInt(input!!0), digitToInt(input!!1))])




-- | This is the getGameMode function, it will input the user for the game strategy of the white and black player.

getGameMode :: IO [String] -- ^ returns list with first element being black player strategy and second element being white player strategy.
getGameMode = do
    putStrLn "Possible strategies: \n  human\n  greedy\n  random\nEnter the strategy for BLACK:"
    blackChoice <-getLine --assign input to blackChoice
            
    putStrLn "Enter the strategy for WHITE:"
    whiteChoice <-getLine --assign input to whiteChoice

    putStrLn ">>>"
    let choices = [blackChoice, whiteChoice]
    return choices --return list of choices








