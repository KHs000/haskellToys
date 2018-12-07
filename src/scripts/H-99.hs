{-
    @Author: Felipe Rabelo
    @Date: Nov 30 2017
    @Last: Dez 06 2018
-}

{-
    Those are my solutions for the 99 problems to be solved in haskell available in
    https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
    
    Currently solving problem: 55
-}

{- Imports used -}
import qualified Data.Function as F
import qualified Data.List     as DL
import qualified Data.Ord      as O
import qualified Data.Ratio    as DR
import qualified Data.Vector   as V
import qualified Control.Monad as M
import qualified System.Random as R

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
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' 1 x xs
encodeDirect' n y [] = [encodeElement n y]
encodeDirect' n y (x:xs) | y == x    = encodeDirect' (n + 1) y xs
                         | otherwise = encodeElement n y : (encodeDirect' 1 x xs)
encodeElement 1 y = Single y
encodeElement n y = Multiple n y

{-
    * Problem 14 -> Duplicate the elements of a list.
    Example in Haskell:

    > dupli [1, 2, 3]
    [1,1,2,2,3,3]
-}
dupli :: [a] -> [a]
dupli = foldl (\acc x -> acc ++ replicate 2 x) []

dupli' :: [a] -> [a]
dupli' = concatMap (replicate 2)

dupli'' :: [a] -> [a]
dupli'' [] = []
dupli'' (x:xs) = x:x:dupli'' xs

dupli''' :: [a] -> [a]
dupli''' list = concat [[x,x] | x <- list]

{-
    * Problem 15 -> Replicate the elements of a list a given number of times.
    Example in Haskell:

    > repli "abc" 3
    "aaabbbccc"
-}
repli :: [a] -> Int -> [a]
repli list n = concatMap (replicate n) list

repli' :: [a] -> Int -> [a]
repli' = flip $ concatMap . replicate

repli'' :: [a] -> Int -> [a]
repli'' xs n = xs >>= replicate n

{-
    * Problem 16 -> Drop every N'th element from a list.
    Example in Haskell:

    *Main> dropEvery "abcdefghik" 3
    "abdeghk"
-}
dropEvery :: [a] -> Int -> [a]
dropEvery list i = V.toList . V.ifilter (\index e -> (index + 1) `mod` i /= 0) $ V.fromList list

{-
    * Problem 17 -> Split a list into two parts; the length of the first part is given. Do not use any 
    predefined predicates.
    Example in Haskell:

    *Main> split "abcdefghik" 3
    ("abc", "defghik")
-}
split :: [a] -> Int -> ([a], [a])
split [] _     = ([], [])
split list 0   = ([], list)
split (x:xs) n = ([x] ++ fst callable, snd callable)
    where callable = split xs (n - 1)
    
split' :: [a] -> Int -> ([a], [a])
split' (x:xs) n | n > 0 = let (f, l) = split xs (n - 1) in (x : f, l)
split' xs _             = ([], xs)
    
{-
    * Problem 18 -> Extract a slice from a list. Given two indices, i and k, the slice is the list 
    containing the elements between the i'th and k'th element of the original list (both limits included).
    Start counting the elements with 1.
    Example in Haskell:

    *Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
    "cdefg"
-}
slice :: [a] -> Int -> Int -> [a]
slice xs i k = let j = i - 1 in take (k - j) $ drop j xs

slice' :: [a] -> Int -> Int -> [a]
slice' xs i k = [x | (x,j) <- zip xs [1..k], i <= j]

{-
    * Problem 19 -> Rotate a list N places to the left. Hint: Use the predefined functions length and (++).
    Examples in Haskell:

    *Main> rotate ['a','b','c','d','e','f','g','h'] 3
    "defghabc"
     
    *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
    "ghabcdef"
-}
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n
    | n > 0 =
        if n >= listLen then rotate xs (n `mod` listLen)
        else drop n xs ++ take n xs
    | n < 0 =
        if o >= listLen then rotate xs (negate $ o `mod` listLen)
        else slice xs (listLen - o + 1) listLen ++ take (listLen - o) xs
    where listLen = length xs
          o = abs n
          
rotate' :: [a] -> Int -> [a]
rotate' xs n = take len . drop (n `mod` len) . cycle $ xs
    where len = length xs
    
rotate'' :: [a] -> Int -> [a]
rotate'' [] _ = []
rotate'' x 0  = x
rotate'' x y
  | y > 0     = rotate (tail x ++ [head x]) (y-1)
  | otherwise = rotate (last x : init x) (y+1)
  
{-
    * Problem 20 -> Remove the K'th element from a list.
    Example in Haskell:

    *Main> removeAt 2 "abcd"
    ('b',"acd")
-}
removeAt :: Int -> [a] -> (Maybe a, Maybe [a])
removeAt _ []     = (Nothing, Nothing)
removeAt 0 (x:xs) = (Just x, Just xs)
removeAt n list   = (Just $ list !! (n - 1), Just [x | (x, i) <- zip list [1..], i /= n])

removeAt' :: Int -> [a] -> (a, [a])
removeAt' k xs = case back of
        []     -> error "removeAt: index too large"
        x:rest -> (x, front ++ rest)
  where (front, back) = splitAt (k - 1) xs
  
removeAt'' :: Int -> [a] -> (a, [a])
removeAt'' n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)

{-
    * Problem 21 -> Insert an element at a given position into a list.
    Example in Haskell:

    P21> insertAt 'X' "abcd" 2
    "aXbcd"
-}
insertAt :: a -> [a] -> Int -> [a]
insertAt e []     0 = [e]
insertAt _ []     _ = []
insertAt e (x:xs) 1 = e : x : xs
insertAt e (x:xs) n = x : insertAt e xs (n - 1)

{-
    * Problem 22 -> Create a list containing all integers within a given range.
    Example in Haskell:

    Prelude> range 4 9
    [4,5,6,7,8,9]
-}
range :: Int -> Int -> [Int]
range a b = case bool of
        True  -> error "range: the initial value is greater than the final"
        False -> [a..b]
    where bool = a > b
    
{-
    * Problem 23 -> Extract a given number of randomly selected elements from a list.
    Example in Haskell:

    Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
    eda
-}
rnd_select :: [a] -> Int -> IO [a]
rnd_select _ 0 = return []
rnd_select (x:xs) n =
    do r <- R.randomRIO (0, (length xs))
       if r < n
           then do
               rest <- rnd_select xs (n-1)
               return (x : rest)
           else rnd_select xs n

rnd_select' :: Show a => [a] -> Int -> IO ()
rnd_select' pool n = do
    g <- R.newStdGen
    let rndIndexs = take n $ R.randomRs (0, (length pool) - 1) g in print $ map (\i -> pool !! i) rndIndexs

{-
    * Problem 24 -> Lotto: Draw N different random numbers from the set 1..M.
    Example in Haskell:

    Prelude System.Random>diff_select 6 49
    Prelude System.Random>[23,1,17,33,21,37]
-}
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
  gen <- R.getStdGen
  return . take n $ R.randomRs (1, m) gen
   
{-
    * Problem 25 -> Generate a random permutation of the elements of a list.
    Example in Haskell:

    Prelude System.Random>rnd_permu "abcdef"
    Prelude System.Random>"badcef"
-}
rnd_permu :: [a] -> IO [a]
rnd_permu xs = rnd_select xs (length xs)

{-
    * Problem 26 -> Generate the combinations of K distinct objects chosen from the N elements of
    a list. In how many ways can a committee of 3 be chosen from a group of 12 people? We all know
    that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients).
    For pure mathematicians, this result may be great. But we want to really generate all the
    possibilities in a list.
    Example in Haskell:

    > combinations 3 "abcdef"
    ["abc","abd","abe",...]
-}
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [ y:ys | y:xs' <- DL.tails xs
                           , ys    <- combinations (n - 1) xs']

{-
    * Problem 27 -> Group the elements of a set into disjoint subsets.
    a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
    Write a function that generates all the possibilities and returns them in a list.
    b) Generalize the above predicate in a way that we can specify a list of group sizes and the
    predicate will return a list of groups.
    
    Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same
    solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...)
    and ((CARLA DAVID) (ALDO BEAT) ...).
    Example in Haskell:

    P27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
    [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
    (altogether 1260 solutions)
-}
group :: [Int] -> [a] -> [[[a]]]
group [] = const [[]]
group (n:ns) = concatMap (uncurry $ (. group ns) . map . (:)) . combination n

combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]
    
{-
    * Problem 28 -> Sorting a list of lists according to length of sublists.
    a) We suppose that a list contains elements that are lists themselves. The
    objective is to sort the elements of this list according to their length.
    E.g. short lists first, longer lists later, or vice versa.
    Example in Haskell:
    
    Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
    Prelude>["o","de","de","mn","abc","fgh","ijkl"]
    
    b) Again, we suppose that a list contains elements that are lists themselves.
    But this time the objective is to sort the elements of this list according to
    their length frequency; i.e., in the default, where sorting is done ascendingly,
    lists with rare lengths are placed first, others with a more frequent length come later.
    Example in Haskell:

    lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
    ["ijkl","o","abc","fgh","de","de","mn"]
-}
lsort :: [[a]] -> [[a]]
lsort = DL.sortBy (O.comparing length)

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . DL.groupBy ((==) `F.on` length) . lsort

{-
    * Problem 31 -> Determine whether a given integer number is prime.
    Example in Haskell:

    P31> isPrime 7
    True
-}

isPrime :: Int -> Bool
isPrime k = k > 1 &&
   foldr (\p r -> p*p > k || k `rem` p /= 0 && r)
      True primesTME

primesTME = 2 : gaps 3 (join [[p*p,p*p+2*p..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p*p,p*p+2*p..] | p <- primes'])
    join  ((x:xs):t)        = x : DL.union xs (join (pairs t))
    pairs ((x:xs):ys:t)     = (x : DL.union xs ys) : pairs t
    gaps k xs@(x:t) | k==x  = gaps (k+2) t 
                    | True  = k : gaps (k+2) xs
                    
{-
    * Problem 32 -> Determine the greatest common divisor of two positive integer numbers.
    Use Euclid's algorithm.
    Example in Haskell:

    [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
    [9,3,3]
-}
myGCD :: Int -> Int -> Int
myGCD x y = if greaterNum `mod` smallerNum == 0 then x else myGCD fstDiff smallerNum
    where greaterNum = if x > y then x else y
          smallerNum = if x == greaterNum then y else x
          fstDiff = greaterNum `mod` smallerNum

myGCD' :: Int -> Int -> Int
myGCD' a b
      | b == 0    = abs a
      | otherwise = myGCD b (a `mod` b)
      
{-
    * Problem 33 -> Determine whether two positive integer numbers are coprime. Two numbers
    are coprime if their greatest common divisor equals 1.
    Example in Haskell:

    * coprime 35 64
    True
-}
coprime :: Int -> Int -> Bool
coprime a b
        | inputGCD == 1 = True
        | otherwise     = False
    where inputGCD = myGCD a b
    
coprime' :: Int -> Int -> Bool
coprime' a b = gcd a b == 1

{-
    * Problem 34 -> Calculate Euler's totient function phi(m). Euler's so-called totient function
    phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
    Example in Haskell:

    * totient 10
    4
-}
totient :: Int -> Int
totient n = length [x | x <- [1..n], coprime x n]
                 
{-
    * Problem 35 -> Determine the prime factors of a given positive integer. Construct a flat list
    containing the prime factors in ascending order.
    Example in Haskell:

    > primeFactors 315
    [3, 3, 5, 7]
-}
primeFactors :: Int -> [Int]
primeFactors a = let (f, f1) = factorPairOf a
                     f' = if prime f then [f] else primeFactors f
                     f1' = if prime f1 then [f1] else primeFactors f1
                 in f' ++ f1'
 where
 factorPairOf a = let f = head $ factors a
                  in (f, a `div` f)
 factors a    = filter (isFactor a) [2..a-1]
 isFactor a b = a `mod` b == 0
 prime a      = null $ factors a
 
{-
    * Problem 36 -> Determine the prime factors of a given positive integer. Construct a list
    containing the prime factors and their multiplicity.
    Example in Haskell:

    *Main> prime_factors_mult 315
    [(3,2),(5,1),(7,1)]
-}
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = map swap . encode $ primeFactors n
    where
        swap (x, y) = (y, x)

{-
    * Problem 37 -> Calculate Euler's totient function phi(m) (improved). See problem 34 for the definition
    of Euler's totient function. If the list of the prime factors of a number m is known in the form of
    problem 36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...)
    be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with
    the following formula:
    
    phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
         (p2 - 1) * p2 ** (m2 - 1) * 
         (p3 - 1) * p3 ** (m3 - 1) * ...
         
    Note that a ** b stands for the b'th power of a.
-}
totient2 :: Int -> Int
totient2 m = product [(p - 1) * p ^ (c - 1) | (p, c) <- prime_factors_mult m]

{-
    * Problem 38 -> Compare the two methods of calculating Euler's totient function. Use the solutions of problems 34 
    and 37 to compare the algorithms. Take the number of reductions as a measure for efficiency. Try to calculate
    phi(10090) as an example.

    (no solution required)
-}

{-
    * Problem 39 -> A list of prime numbers. Given a range of integers by its lower and upper limit, construct a list 
    of all prime numbers in that range.
    Example in Haskell:
    
    P29> primesR 10 20
    [11,13,17,19]
-}
primeR :: Int -> Int -> [Int]
primeR a b = [x | x <- [a..b], isPrime x]

{-
    * Problem 40 -> Goldbach's conjecture. Goldbach's conjecture says that every positive even number greater than
    2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory
    that has not been proved to be correct in the general case. It has been numerically confirmed up to very large
    numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers
    that sum up to a given even integer.
    Example in Haskell:

    *goldbach 28
    (5, 23)
-}
goldbach :: Int -> (Int, Int)
goldbach m = goldHelper m $ primeR 1 m

goldHelper :: Int -> [Int] -> (Int, Int)
goldHelper _ [] = (0, 0)
goldHelper m (x:xs)
    | isPrime (m - x) = (x, m - x)
    | otherwise       = goldHelper m xs


{-
    * Problem 41 -> Given a range of integers by its lower and upper limit, print a list of all even numbers and
    their Goldbach composition. In most cases, if an even number is written as the sum of two prime numbers, one
    of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases
    there are in the range 2..3000.
    Example in Haskell:

    *Exercises> goldbachList 9 20
    [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
    *Exercises> goldbachList' 4 2000 50
    [(73,919),(61,1321),(67,1789),(61,1867)]
-}
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = map goldbach [x | x <- [a..b], even x]

goldbachList':: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b m = filter (\t -> fst t >= m) $ goldbachList a b

{- 
    At this point, the list jumps from problem 41 to problem 46. I guessing that's because some previous problems
    had more than 1 part to it, so those ones actually counted as multiple exercises. From this point forward, I'll
    not be commenting over when this kinda of situation happens again.
-}

{-
    * Problem 46 -> Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence)
    which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if
    and only if both A and B succeed. A logical expression in two variables can then be written as in the following
    example: and(or(A,B),nand(A,B)). Now, write a predicate table/3 which prints the truth table of a given logical 
    expression in two variables.
    Example in Haskell:

    > table (\a b -> (and' a (or' a b)))
    True True True
    True False True
    False True False
    False False False
-}
and'  a b = a && b
or'   a b = a || b
nand' a b = not (and' a b)
nor'  a b = not (or' a b)
xor'  a b = not (equ' a b)
impl' a b = or' (not a) b
equ'  a b = a == b

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b)
                                | a <- [True, False], b <- [True, False]]

{-
    * Problem 47 -> Truth tables for logical expressions (2). Continue problem P46 by defining and/2, or/2, etc as 
    being operators. This allows to write the logical expression in the more natural way, as in the example: A and 
    (A or not B). Define operator precedence as usual; i.e. as in Java.
    Example in Haskell:

    > table2 (\a b -> a `and'` (a `or'` not b))
    True True True
    True False True
    False True False
    False False False
-}
infixl 4 `or'`
infixl 6 `and'`

{-
    * Problem 48 -> Truth tables for logical expressions (3). Generalize problem P47 in such a way that the logical 
    expression may contain any number of logical variables. Define table/2 in a way that table(List,Expr) prints the 
    truth table for the expression Expr, which contains the logical variables enumerated in List.
    Example in Haskell:

    > tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
    -- infixl 3 `equ'`
    True  True  True  True
    True  True  False True
    True  False True  True
    True  False False True
    False True  True  True
    False True  False True
    False False True  True
    False False False True
-}
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `nand'`
infixl 3 `equ'`
 
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [toStr a ++ " => " ++ show (f a) | a <- args n]
    where args n = M.replicateM n [True, False]
          toStr  = unwords . map (\x -> show x ++ space x)
          space True = "  "
          space False = " "

{-
    * Problem 49 -> Gray codes. An n-bit Gray code is a sequence of n-bit strings constructed according to certain 
    rules. Find out the construction rules and write a predicate with the following specification:
    
    % gray(N,C) :- C is the N-bit Gray code

    Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be 
    used repeatedly?
    Example in Haskell:

    P49> gray 3
    ["000","001","011","010","110","111","101","100"]
-}
gray :: Int -> [String]
gray 0 = [""]
gray n = foldr (\s acc -> ("0" ++ s):("1" ++ s):acc) [] $ gray (n-1)

{-
    * Problem 50 -> Huffman codes. We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. 
    Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct a list hc(S,C) terms, 
    where C is the Huffman code word for the symbol S. In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), 
    hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall be performed by the predicate 
    huffman/2 defined as follows:

    % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs

    Example in Haskell:

    *Exercises> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
    [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
-}
huffman :: [(Char, Int)] -> [(Char, [Char])]
huffman x = reformat $ huffman_combine $ resort $ morph x
    where
        morph x = [ ([[]],[c],n) | (c,n) <- x ]
        resort x = DL.sortBy (\(_,_,a) (_,_,b) -> compare a b) x
 
        reformat (x,y,_) = DL.sortBy (\(a,b) (x,y) -> compare (length b) (length y)) $ zip y x
 
        huffman_combine (x:[]) = x
        huffman_combine (x:xs) = huffman_combine $ resort ( (combine_elements x (head xs)) : (tail xs) )
            where
                combine_elements (a,b,c) (x,y,z) = ( (map ('0':) a) ++ (map ('1':) x), b ++ y, c+z)

{-
    * Problem 55 -> Construct completely balanced binary trees. In a completely balanced binary tree, the following property 
    holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost 
    equal, which means their difference is not greater than one.Write a function cbal-tree to construct completely balanced 
    binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 
    'x' as information into all nodes of the tree.
-}
