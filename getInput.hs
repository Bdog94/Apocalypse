import Control.Monad
import System.Exit(exitSuccess)
import System.Environment   
import Data.List  
import System.Console.GetOpt
import Prelude hiding ( catch )
import Control.Exception
import Control.Monad ( liftM )
import System.IO


--This function will get arguments from the command line, if the user does not enter the arguments in the command line, 
--it will prompt them for an input. If the user doesnt type in human or greedy, the program will terminate. 

--Note : The function does not compile if I do not use main, I will fix it so that it returns the given strings for blackInput or whiteInput
getGameMode :: [String] = do 
    argList <- getArgs
    let blackChoice = head argList
    let whiteChoice = argList!! 1
    if (argList == []) then
		argsNotGiven else
			if (blackChoice /= "greedy" && blackChoice /= "human" && whiteChoice /= "greedy" && whiteChoice /= "human") then
				   putStrLn "  human\n  greedy" else
						putStrLn ">>>"
						getGameMode = [blackChoice, whiteChoice]
    
argsNotGiven = do
    putStrLn "Possible strategies: \n\thuman\n\tgreedy\nEnter the strategy for BLACK:"
    blackChoice <-getLine
    when (blackChoice /= "greedy" && blackChoice /= "human") $ do 
            putStrLn "  human"
            putStrLn "  greedy"
            exitSuccess
            
    putStrLn "Enter the strategy for WHITE:"
    whiteChoice <-getLine
    when (whiteChoice /= "greedy" && whiteChoice /= "human") $ do 
            putStrLn "  human"
            putStrLn "  greedy"
            exitSuccess
    putStrLn ">>>"
