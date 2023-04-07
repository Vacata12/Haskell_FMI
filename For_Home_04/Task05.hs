main :: IO()
main = do
    print $ isAscending 0 == True
    print $ isAscending 10 == False
    print $ isAscending 123 == True
    print $ isAscending 1233 == True
    print $ isAscending 12332 == False

isAscending :: Int -> Bool
isAscending num
 | num < 0 = False 
 | num < 10 = True 
 | (head $ show num) > (head $ tail $ show num) = False
 | otherwise = isAscending (read $ tail $ show num)