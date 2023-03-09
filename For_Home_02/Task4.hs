countOccurences :: Int -> Int -> Int
countOccurences n m = helper n m 0
 where
     helper 0 m result = result
     helper n m result
      |mod n 10 == m = helper (div n 10) m (result + 1)
      |otherwise = helper (div n 10) m result

main :: IO()
main = do
    print $ countOccurences 121 1 == 2
    print $ countOccurences 222 1 == 0