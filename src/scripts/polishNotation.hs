{-
    @Author: Felipe Rabelo
    @Date: Nov 7 2017
-}

{-
    Example expression: "( 20 - ( 4 / 2 * ( 1 + 4 ) ) + ( 1 - 1 ) ) / 5"
    The above expression must resolve to the RPN: "20 1 4 + 4 2 / * - 1 1 - + 5 /"
    Which will resolve to 2 using the solveRPN function
-}

import Data.List

{-
    Must go through the expression, searching for the an '(', then do a takeWhile /= '(' || ')'. If it finds an
    '(', it must recursively call itself to turn the content of the innermost () to the RPN. Then it must do the 
    same with the supported operations, in the same precedence order.
-}
toRPN :: String -> String
toRPN [] = []
toRPN = foldl findSubExpr [] . words
    where   findSubExpr (x:xs) "(" = toRPN . takeWhile (/= "(" || /= ")") xs {-TODO think carefully about this predicate-}
            findSubExpr xs numberString= numberString:xs
  
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