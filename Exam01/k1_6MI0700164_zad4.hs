main :: IO()
main = do
    print $ prefixToSuffix [1, 3, 6, 10, 15] == [15, 14, 12, 9, 5]
    print $ prefixToSuffix [1, 3, 6, 10, 15] == [15, 14, 12, 9, 5]
    print $ prefixToSuffix [0] == [0]
    print $ prefixToSuffix [-1, -2, -3, -4, -5] == [-5, -4, -3, -2, -1]
    print $ prefixToSuffix [1, -4, 2, 90, 100, -1] == [-1, -2, 3, -3, -91, -101]
    print $ prefixToSuffix [1, 0, 1, 0, 1, 0, 1, 0] == [0, -1, 0, -1, 0, -1, 0, -1]
    print $ prefixToSuffix [0, 0, 0, 0, 0, 0, 0, 0, 0, 1] == [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    print $ prefixToSuffix [0, 0, 0, 0, 0, 0, 1, 1, 1, 1] == [1, 1, 1, 1, 1, 1, 1, 0, 0, 0]

prefixToSuffix :: (Num a) => [a] -> [a]
prefixToSuffix [x] = [x]
prefixToSuffix xs = helper xs (head $ (reverse xs)) (length xs)
 where
    helper copyXs last follower
     | length copyXs == 1 = []
     | length copyXs == follower = last : helper copyXs last (follower - 1) 
     | otherwise = last - (head copyXs) : helper (tail copyXs) last (follower - 1)

