import Data.Ref

data List a = Empty | Node (List a) a (List a) deriving (Show, Read, Eq, Ord)

singleton :: a -> List a
singleton value = Node Empty value Empty