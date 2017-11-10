{-
    @Author: Felipe Rabelo
    @Date: Nov 7 2017
    @Last: Nov 10 2017
-}

{-
    Example expression: "( 20 - ( 4 / 2 * ( 1 + 4 ) ) + ( 1 - 1 ) ) / 5"
    The above expression must resolve to the RPN: "20 1 4 + 4 2 / * - 1 1 - + 5 /"
    Which will resolve to 2 using the solveRPN function
-}

import Data.List

toRPN :: [String] -> String
toRPN = foldl findSubExpr ""
    where   findSubExpr "" "(" = ""
            findSubExpr "" numStr = "" ++ numStr
            findSubExpr acc@(x:xs) "(" = takeWhileEither (/='(') (\xs -> acc ++ (toRPN $ words xs)) (/=')') (\acc -> acc) xs
            findSubExpr acc numStr = if numStr == ")" then acc else acc ++ " " ++ numStr

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