{-
    @Author: Felipe Rabelo
    @Date: Oct 27 2017
-}

{-
   This module is kind of useless, provided that an implemantation of an actual linked list,
   would conflitct with one of the fundamental functional programming paradigms (the 
   immutability one). Therefor, this module is nothing but a toy module with the sole purpose
   of being a study object on datatypes and its behavior.
-}

module List
( insertL
, insertR
) where

data List a = Empty | Node (List a) a (List a) deriving (Show, Read, Eq, Ord)

singleton :: a -> List a
singleton value = Node Empty value Empty

singletonL :: a -> List a -> List a
singletonL value prevValue = Node Empty value prevValue

singletonR :: a -> List a -> List a
singletonR value nextValue = Node nextValue value Empty

insertL :: (Ord a) => a -> List a -> List a
insertL value Empty = singleton value
insertL value prevNode@(Node left a right) = Node (insertL' value left prevNode) a right

insertL' :: (Ord a) => a -> List a -> List a -> List a
insertL' value Empty rightNode@(Node left a right) = singletonL value rightNode
insertL' value prevNode@(Node left a right) _ = Node (insertL' value left prevNode) a right

insertR :: (Ord a) => a -> List a -> List a
insertR value Empty = singleton value
insertR value nextNode@(Node left a right) = Node left a (insertR' value right nextNode)

insertR' :: (Ord a) => a -> List a -> List a -> List a
insertR' value leftNode@(Node left a right) Empty = singletonR value leftNode
insertR' value _ nextNode@(Node left a right) = Node left a (insertR' value nextNode right)