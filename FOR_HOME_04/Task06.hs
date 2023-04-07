main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392 -- n = 5, d = 2
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum $ take n [x | x <- [1..], digitIsIn x d, isPrime x]

digitIsIn :: Int -> Int -> Bool
digitIsIn 0 _ = False
digitIsIn num1 digit
 | mod num1 10 == digit = True
 | otherwise = digitIsIn (div num1 10) digit

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = helper n 3
 where
     helper num d
      | d == num = True
      | mod num d == 0 = False
      | otherwise = helper num (d + 1)
