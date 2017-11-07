{-
    @Author: Felipe Rabelo
    @Date: Nov 7 2017
-}

{-
    Example expression: "(20 - ( 4 / 2 * ( 1 + 4 ) ) + ( 1 - 1 )) / 5"
    The above expression must resolve to the RPN: "20 1 4 + 4 2 / * - 1 1 - + 5 /"
    Which will resolve to 2
-}

import Data.List

-- toRPN :: String -> String
-- toRPN = words
  
solveRPN :: String -> Float  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = read numberString:xs  