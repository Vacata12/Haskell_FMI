main :: IO()
main = do
     print $ eqSumPowDig 100 2 == 0
     print $ eqSumPowDig 1000 2 == 0
     print $ eqSumPowDig 2000 2 == 0
     print $ eqSumPowDig 200 3 == 153
     print $ eqSumPowDig 370 3 == 523
     print $ eqSumPowDig 370 3 == 523
     print $ eqSumPowDig 400 3 == 894
     print $ eqSumPowDig 500 3 == 1301
     print $ eqSumPowDig 1000 3 == 1301
     print $ eqSumPowDig 1500 3 == 1301
     print $ getNthSevenlikeNum 1 == 1
     print $ getNthSevenlikeNum 2 == 7
     print $ getNthSevenlikeNum 3 == 8
     print $ getNthSevenlikeNum 4 == 49

--Task01
eqSumPowDig :: Int -> Int -> Int
eqSumPowDig max pow = helper max pow 2 0
 where
    helper max pow num result
     | max < num = result
     | num == (specialNum num pow) = helper max pow (num + 1) result + num 
     | otherwise = helper max pow (num + 1) result

specialNum :: Int -> Int -> Int
specialNum num pow = helper num pow 0
 where
     helper 0 pow result = result
     helper num pow result = helper (div num 10) pow (result + (mod num 10) ^ pow)

--Task02
getNthSevenlikeNum :: Int -> Int
getNthSevenlikeNum 1 = 1 
getNthSevenlikeNum n 
 | n <= 0 = error "Negative num! Num must be positive" 
 | mod n 2 == 1 = 1 + 7 * getNthSevenlikeNum(div n 2) 
 | otherwise = 7 * getNthSevenlikeNum(div n 2)