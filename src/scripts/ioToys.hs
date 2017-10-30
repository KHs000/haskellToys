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
            putStrLn "Give me an it: "  
            subj <- getLine
            putStrLn $ subj ++ " is " ++ adj
            return ())
        return ()
    else do
    if program == "toisen"
    then do
        coefs <- forM ["a", "b", "c"] (\coefficient -> do
            putStrLn $ "Choose a value for the coefficient " ++ coefficient
            value <- getLine
            return value)
        let a = read $ coefs !! 0 :: Float
            b = read $ coefs !! 1 :: Float
            c = read $ coefs !! 2 :: Float
            roots = secondDegreeEquation a b c
        putStrLn $ "The first root is " ++ show (fst roots) ++ " and the second one is " ++ show (snd roots)
        return ()
    else do
        putStrLn "We don't have this program, yet! How about implementing it?!"
        return ()
        
secondDegreeEquation :: (Floating a) => a -> a -> a -> (a, a)
secondDegreeEquation a b c = (root, root')
    where root = ((-b + sqrt delta) / (2 * a))
          root' = ((-b - sqrt delta) / (2 * a))
          delta = b * b - 4 * a * c