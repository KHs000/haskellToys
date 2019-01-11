{-
    @Author: Felipe Rabelo
    @Date: Jan 10 2019
-}

data FB a = Fizz | Buzz | FizzBuzz | Val a deriving (Show)

fizzBuzz :: Int -> Int -> IO ()
fizzBuzz a b = mapM_ print' [if x `mod` 15 == 0 then FizzBuzz 
    else if x `mod` 5 == 0 then Buzz
    else if x `mod` 3 == 0 then Fizz else Val x | x <- [a..b]]

print' :: (Show a) => FB a -> IO ()
print' (Val x) = print x
print' m       = print m
