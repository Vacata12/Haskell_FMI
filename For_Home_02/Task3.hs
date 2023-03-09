isPrime :: Int -> Bool
isPrime n = n > 1 && helper 2
 where
    helper d
     | d >= n = True
     | mod n d == 0 = False
     | otherwise = helper (d + 1)

sumPrimeDivs :: Int -> Int
sumPrimeDivs n = helper n 2 0
 where
    helper num d result
     |isPrime d && mod num d == 0  = helper num (d + 1) result + d
     |d == num = result
     |otherwise = helper num (d + 1) result

main :: IO()
main = do
    print $ sumPrimeDivs 0 == 0
    print $ sumPrimeDivs 6 == 5 -- 2 + 3
    print $ sumPrimeDivs 18 == 5 -- 2 + 3
    print $ sumPrimeDivs 19 == 19
    print $ sumPrimeDivs 45136 == 53
