main :: IO()
main = do
      print $ getPalindromes 132465 == 8
      print $ getPalindromes 654546 == 8
      print $ getPalindromes 100001 == 100012
      print $ getPalindromes 21612 == 21614
      print $ getPalindromes 26362 == 26364   

getPalindromes :: Int -> Int
getPalindromes num = let xs = [x | x <- [2 .. num], isPolindrom x, mod num x == 0 ] in minimum xs + maximum xs

isPolindrom :: Int -> Bool
isPolindrom num = num == (rev num)

rev :: Int -> Int
rev num = helper num 0
 where
     helper :: Int -> Int -> Int
     helper num res
      | num < 10 = res * 10 + num
      | otherwise = helper (div num 10) (res * 10 + mod num 10)

smallest :: Int -> Int
smallest num = let xs = [x | x <- [2 .. num], isPolindrom x, mod num x == 0 ] in minimum xs

biggest :: Int -> Int
biggest num = let xs = [x | x <- [2 .. num], isPolindrom x, mod num x == 0 ] in maximum xs