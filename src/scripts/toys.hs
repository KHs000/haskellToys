secondDegree :: (Floating a) => a -> a -> a -> (a, a)
secondDegree a b c = (root, root')
    where   root = ((-b + sqrt delta) / (2 * a))
            root' = ((-b - sqrt delta) / (2 * a))
            delta = b * b - 4 * a * c
          
takeWhileEither :: (a -> Bool) -> ([a] -> [a]) -> (a -> Bool) -> ([a] -> [a]) -> [a] -> [a]
takeWhileEither _ _ _ _ [] = []
takeWhileEither p f p' f' list@(x:xs) = foldr (\x acc -> 
                                                            if p x && (not $ p' x) {-TODO refactor this predicate so the function may work properly-}
                                                            then takeWhile p list
                                                            else if p' x
                                                            then takeWhile p' list else acc) [] list