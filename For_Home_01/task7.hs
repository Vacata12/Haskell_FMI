growingPlant :: Int -> Int -> Int -> Int
growingPlant x y z
 | z <= x = 1
 | otherwise = 1 + growingPlant x y (z - x + y)

main :: IO()
main = do
    print $ growingPlant 5 2 5 == 1
    print $ growingPlant 5 2 6 == 2
    print $ growingPlant 10 9 4 == 1
    print $ growingPlant 100 10 910 == 10