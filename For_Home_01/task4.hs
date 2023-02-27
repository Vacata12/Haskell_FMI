myGcdG :: Int -> Int -> Int
myGcdG n m
 |n == 0 = m
 |m == 0 = n
 |otherwise = myGcdG m (mod n m)

myGcdPM :: Int -> Int -> Int
myGcdPM 0 m = m
myGcdPM n 0 = n
myGcdPM n m = myGcdG m (mod n m)

main :: IO()
main = do
    print $ myGcdG 5 13 == 1
    print $ myGcdG 13 1235 == 13
    print $ myGcdPM 5 13 == 1
    print $ myGcdPM 13 1235 == 13

 