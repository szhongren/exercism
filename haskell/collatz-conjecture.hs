module CollatzConjecture
    ( collatz
    ) where

collatz :: Integer -> Maybe Integer
collatz = collatz' 0

collatz' :: Integer -> Integer -> Maybe Integer
collatz' acc 1 = Just acc
collatz' acc n | n <= 0    = Nothing
               | even n    = collatz' (acc + 1) (n `div` 2)
               | otherwise = collatz' (acc + 1) (3 * n + 1)

