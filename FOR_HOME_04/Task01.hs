main :: IO()
main = do
    print $ mySumRecNonPM [] == 0
    print $ mySumRecNonPM [1, 2, 3] == 6

    print $ mySumRecPM [] == 0
    print $ mySumRecPM [1, 2, 3] == 6

    print $ mySumFunc [] == 0
    print $ mySumFunc [1, 2, 3] == 6

mySumRecNonPM :: [Int] -> Int
mySumRecNonPM xs
 | length xs == 0 = 0
 | otherwise = sum xs

mySumRecPM :: [Int] -> Int
mySumRecPM [] = 0
mySumRecPM (x:xs) = x + mySumRecPM xs

mySumFunc :: [Int] -> Int
mySumFunc = sum 