countPalindromes :: Int -> Int -> Int
countPalindromes n m = helper (n + 1) m 0
 where
     helper n m result
      | n == m = result
      | n == myPolindrom n = helper (n + 1) m (result + 1)
      | otherwise = helper (n + 1) m result

myPolindrom :: Int -> Int
myPolindrom n = helper n 0
 where
     helper 0 result = result
     helper leftover result = helper (div leftover 10) (result * 10 + (mod leftover 10))

main :: IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11
