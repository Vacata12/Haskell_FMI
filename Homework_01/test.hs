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

eqSumPowDig :: Int -> Int -> Int
eqSumPowDig hMax power
 | hMax < 1 = error "Invalid number"
 | power < 1 = error "Invalid power"
 | otherwise = helper 2 0
     where
        helper n result
          | n > hMax = result
          | sumDigitsToPower n power == n = helper (n + 1) (result + n)
          | otherwise = helper (n + 1) result
 
 
sumDigitsToPower :: Int -> Int -> Int
sumDigitsToPower n power 
 | n < 0 = error "n was negative"
 | otherwise = helper n 0
  where
    helper 0 result = result
    helper leftover result = helper (div leftover 10) (result + (mod leftover 10)^power)