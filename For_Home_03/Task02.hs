main :: IO()
main = do
    print $ sortN 1714 == 7411
    print $ sortN 123450 == 543210
    print $ sortN 123405 == 543210
    print $ sortN 123045 == 543210
    print $ sortN 120345 == 543210
    print $ sortN 102345 == 543210
    print $ sortN 8910 == 9810
    print $ sortN 321 == 321
    print $ sortN 29210 == 92210
    print $ sortN 1230 == 3210
    print $ sortN 55345 == 55543
    print $ sortN 14752 == 75421
    print $ sortN 329450 == 954320
    print $ sortN 9125 == 9521

sortN :: Int -> Int
sortN 0 = 0
sortN num = biggestNum num * (10 ^ ((digits num) - 1)) + sortN (removeFistOccurrence num $ biggestNum num)
    

biggestNum :: Int -> Int
biggestNum num = helper num 0
 where
    helper 0 res = res
    helper n res
     | mod n 10 > res = helper (div n 10) (mod n 10)
     | otherwise = helper (div n 10) res

digits :: Int -> Int
digits 0 = 0
digits num = 1 + digits (div num 10)

removeFistOccurrence :: Int -> Int -> Int
removeFistOccurrence n d
    | n < 10 && n == d = 0
    | mod n 10 == d = div n 10
    | otherwise = removeFistOccurrence (div n 10) d * 10 + mod n 10