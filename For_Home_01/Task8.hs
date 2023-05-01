main :: IO()
main = do
    print $ snail 3 2 1 == 2
    print $ snail 10 3 1 == 5
    print $ snail 10 3 2 == 8
    print $ snail 100 20 5 == 7
    print $ snail 5 10 3 == 1
    print $ snail 5 5 2 == 1

snail :: Int -> Int -> Int -> Int
snail column day night
 | day <= night = 1
 | otherwise = ceiling $ fromIntegral (column - night) / fromIntegral (day - night)
