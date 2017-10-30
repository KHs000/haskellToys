secondDegree :: (Floating a) => a -> a -> a -> (a, a)
secondDegree a b c = (root, root')
    where root = ((-b + sqrt delta) / (2 * a))
          root' = ((-b - sqrt delta) / (2 * a))
          delta = b * b - 4 * a * c