everyOther :: Int -> Int
everyOther n = helper n 1 0
 where
     helper 0 times result = result
     helper n times result
      | mod times 2 == 0 = helper (div n 10) (times + 1) (result * 10 + (mod n 10))
      | otherwise = helper (div n 10) (times + 1) result

main :: IO()
main = do
    print $ everyOther 12 == 1
    print $ everyOther 852369 == 628
    print $ everyOther 1714 == 11
    print $ everyOther 12345 == 42
    print $ everyOther 891 == 9
    print $ everyOther 123 == 2
    print $ everyOther 2121 == 22
    print $ everyOther 4736778 == 767
    print $ everyOther 448575 == 784
    print $ everyOther 4214 == 14