{-
    @Author: Felipe Rabelo
    @Date: Nov 30 2017
    @Last: Dez 19 2017
-}

{-
    Those are my solutions for the 99 problems to be solved in haskell available in
    https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
    
    Currently solving problem: 13
-}

{- Imports used -}
import qualified Data.List as DL

{- Data types created -}
data NestedList a = Elem a | List [NestedList a]
data ListItem a = Single a | Multiple Int a deriving (Show)

{-
    * Problem 1 -> Find the last element of a list.
    Example in Haskell:

    Prelude> myLast [1,2,3,4]
    4
-}
myLast :: [a] -> Maybe a
myLast []     = Nothing
myLast [x]    = Just x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' = last

{-
    * Problem 2 -> Find the last but one element of a list.
    Example in Haskell:

    Prelude> myButLast [1,2,3,4]
    3
-}
myButLast :: [a] -> Maybe a
myButLast []  = Nothing
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
    | otherwise     = Just $ xs !! (i - 1)

{-
    * Problem 4 -> Find the number of elements of a list.
    Example in Haskell:

    Prelude> myLength [123, 456, 789]
    3
-}
myLength :: [a] -> Int
myLength = length

{-
    * Problem 5 -> Reverse a list.
    Example in Haskell:

    Prelude> myReverse "A man, a plan, a canal, panama!"
    "!amanap ,lanac a ,nalp a ,nam A"
-}
myReverse :: [a] -> [a]
myReverse = reverse

{-
    * Problem 6 -> Find out whether a list is a palindrome. A palindrome can be read forward or 
    backward; e.g. (x a m a x).
    Example in Haskell:

    *Main> isPalindrome [1,2,3]
    False
-}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = foldr (\bool isP -> bool && isP) True . zipWith (\x y -> x == y) xs $ reverse xs

{-
    * Problem 7 -> Flatten a nested list structure. Transform a list, possibly holding lists as elements
    into a `flat` list by replacing each list with its elements (recursively).
    Example in Haskell:

    We have to define a new data type, because lists in Haskell are homogeneous.
    
    data NestedList a = Elem a | List [NestedList a]
    *Main> flatten (Elem 5)
    [5]
    *Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
    [1,2,3,4,5]
-}
flatten :: NestedList a -> [a]
flatten (Elem x)  = x:[]
flatten (List xs) = concatMap flatten xs

{-
    * Problem 8 -> Eliminate consecutive duplicates of list elements.If a list contains repeated
    elements they should be replaced with a single copy of the element. The order of the elements
    should not be changed.
    Example in Haskell:

    > compress "aaaabccaadeeee"
    "abcade"
-}
compress :: Eq a => [a] -> [a]
compress xs = foldr (\x acc -> if head acc == x then acc else x:acc) [last xs] xs

compress' :: Eq a => [a] -> [a]
compress' = map head . DL.group

{-
    * Problem 9 -> Pack consecutive duplicates of list elements into sublists. If a list contains 
    repeated elements they should be placed in separate sublists.
    Example in Haskell:

    *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
    ["aaaa","b","cc","aa","d","eeee"]
-}
pack :: (Eq a) => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)

{-
    * Problem 10 -> Run-length encoding of a list. Use the result of problem P09 to implement 
    the so-called run-length encoding data compression method. Consecutive duplicates of elements 
    are encoded as lists (N E) where N is the number of duplicates of the element E.
    Example in Haskell:

    encode "aaaabccaadeeee"
    [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}
encode :: Eq a => [a] -> [(Int, a)]
encode []          = []
encode list@(x:xs) = (length $ takeWhile (== x) list, x) : encode (dropWhile (== x) xs)

encode' ::Eq a => [a] -> [(Int, a)]
encode' xs = map (\x -> (length x,head x)) (DL.group xs)

encode'' :: Eq a => [a] -> [(Int, a)]
encode'' = map (\x -> (length x, head x)) . DL.group

{-
    * Problem 11 -> Modified run-length encoding. Modify the result of problem 10 in such a way
    that if an element has no duplicates it is simply copied into the result list. Only elements 
    with duplicates are transferred as (N E) lists.
    Example in Haskell:

    P11> encodeModified "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
-}
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map (\x -> resolveElement (length x) (head x)) . DL.group
    where 
        resolveElement 1 x = Single x
        resolveElement n x = Multiple n x

encodeModified' :: Eq a => [a] -> [ListItem a]
encodeModified' = map encodeHelper . encode
    where
      encodeHelper (1, x) = Single x
      encodeHelper (n, x) = Multiple n x

{-
    * Problem 12 -> Decode a run-length encoded list. Given a run-length code list generated as 
    specified in problem 11. Construct its uncompressed version.
    Example in Haskell:
    
    P12> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
    "aaaabccaadeeee"
-}
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
    where
        decodeHelper (Single x) = [x]
        decodeHelper (Multiple n x) = replicate n x
        
{-
    * Problem 13 -> Run-length encoding of a list (direct solution). Implement the so-called run-length
    encoding data compression method directly. I.e. don't explicitly create the sublists containing the
    duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by
    replacing the singleton lists (1 X) by X.
    Example in Haskell:

    P13> encodeDirect "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-}
encodeDirect :: [a] -> [ListItem a]
