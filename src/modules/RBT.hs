{-
    @Author: Felipe Rabelo
    @Date: Dec 07 2018
-}

module RBT where

data Color   = R | B deriving (Eq, Show, Read)
data Tree  a = E | T Color (Tree a) a (Tree a) deriving (Eq, Show, Read)

member :: (Ord a) => a -> Tree a -> Bool
member x E    = False
member x (T _ a y b)
    | x < y     = member x a
    | x == y    = True
    | otherwise = member x b

insert :: (Ord a) => a -> Tree a -> Tree a
insert x s = makeBlack $ ins s
    where 
        ins E  = T R E x E
        ins (T color a y b)
            | x < y  = balance color (ins a) y b
            | x == y = T color a y b
            | x > y  = balance color a y (ins b)
        makeBlack (T _ a y b) = T B a y b

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b
