main :: IO()
main = do
    print $ numContentChildren [1,2, 3] [1, 1] == 1
    print $ numContentChildren [1, 2] [1, 2, 3] == 2

childOk :: Int -> [Int] -> Bool
childOk g ss
 | null ss = False
 | g <= (head ss) = True
 | otherwise = childOk g (tail ss)

numContentChildren :: [Int] -> [Int] -> Int
numContentChildren gs ss
 | null gs = 0
 | childOk (head gs) ss = 1 +  numContentChildren (tail gs) ss
 | otherwise = 0 + numContentChildren (tail gs) ss
