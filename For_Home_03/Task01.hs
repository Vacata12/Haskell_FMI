main :: IO()
main = do
    print $ removeFistOccurrence 110 1 == 10
    print $ removeFistOccurrence 15365 5 == 1536
    print $ removeFistOccurrence 15360 0 == 1536
    print $ removeFistOccurrence 15300 0 == 1530
    print $ removeFistOccurrence 15365 1 == 5365
    print $ removeFistOccurrence 35365 3 == 3565
    print $ removeFistOccurrence 1212 1 == 122
    print $ removeFistOccurrence 1212 2 == 121
    print $ removeFistOccurrence (removeFistOccurrence 1212 1) 1 == 22

removeFistOccurrence :: Int -> Int -> Int
removeFistOccurrence n d
 | n < 10 && n == d = 0
 | mod n 10 == d = div n 10
 | otherwise = removeFistOccurrence (div n 10) d * 10 + mod n 10