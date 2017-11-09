{-
    @Author: Felipe Rabelo
    @Date: Nov 7 2017
    @Last: Nov 8 2017
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
toRPN :: [String] -> [String]
toRPN = reverse . foldl findSubExpr []
    where   findSubExpr (x:xs) "(" = takeWhileEither (/="(") (\xs -> (toRPN xs) ++ xs) (/=")") (\xs -> xs) xs
            findSubExpr xs numStr = if numStr == ")" then xs else numStr:xs

takeWhileEither :: (a -> Bool) -> ([a] -> [a]) -> (a -> Bool) -> ([a] -> [a]) -> [a] -> [a]
takeWhileEither _ _ _ _ [] = []
takeWhileEither p f p' f' list@(x:xs) = foldr (\x acc -> 
                                                        if (not $ p x) && p' x
                                                        then f $ takeWhile p list
                                                        else if not $ p' x
                                                        then f' $ takeWhile p' list else acc) [] list
  
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