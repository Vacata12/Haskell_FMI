sumOfDigits :: Int -> Int
sumOfDigits n = helper n 0
 where
    helper 0 result = result
    helper leftover result = helper (div leftover 10) (result + (mod leftover 10))

isInteresting :: Int -> Bool
isInteresting n
 |mod n (sumOfDigits n) == 0 = True
 |otherwise = False

main :: IO()
main = do
    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False  
    print $ isInteresting 70 == True 
    print $ isInteresting 5 == True 
    print $ isInteresting 4 == True 