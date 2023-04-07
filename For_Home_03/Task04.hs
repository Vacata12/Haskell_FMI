main :: IO()
main = do
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990

sumDivisibleNumbers :: Int -> Int -> Int -> Int
sumDivisibleNumbers n m k = helper (min n m) (max n m) k
 where
    helper start finish k
     | start == finish = 0
     | mod (sumDigits start) k == 0 = start + helper (start + 1) finish k
     | otherwise = helper (start + 1) finish k

sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits n = mod n 10 + sumDigits (div n 10)