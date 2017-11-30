{-
    @Author: Felipe Rabelo
    @Date: Nov 30 2017
-}

{-
    Those are my solutions for the 99 problems to be solved in haskell available in 
    https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
    
    Currently solving problem: 4
-}

{-
    * Problem 1 -> Find the last element of a list.
    Example in Haskell:

    Prelude> myLast [1,2,3,4]
    4
-}
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' xs = last xs

{-
    * Problem 2 -> Find the last but one element of a list.
    Example in Haskell:

    Prelude> myButLast [1,2,3,4]
    3
-}
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [x] = Nothing
myButLast (x:xs)
    | length xs == 1 = Just x
    | otherwise      = myButLast xs
    
myButLast' :: [a] -> a
myButLast' = last . init

{-
    * Problem 3 -> Find the K'th element of a list. The first element in the list is number 1.
    
    Example in Haskell:

    Prelude> elementAt [1,2,3] 2
    2
-}
elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt xs i
    | length xs < i = Nothing
    | otherwise           = Just $ xs !! (i - 1)
    
{-
    
-}