{-
    @Author: Felipe Rabelo
    @Date : Oct 30 2017
-}

import Control.Monad  
  
main = do
    putStrLn "Tip the name of the program you'd like to run"
    program <- getLine
    if program == "describingThings"
    then do
        forM ["pretty", "boring", "average"] (\adj -> do  
            putStrLn $ "Give me an it: "  
            subj <- getLine
            putStrLn $ subj ++ " is " ++ adj
            return ())
        return ()
    else do
        putStrLn "We don't have this program, yet! How about implementing it?!"
        return ()