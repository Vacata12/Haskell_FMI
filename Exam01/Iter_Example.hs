main :: IO()
main = do
    print $ rev 123
    print $ rev 987654321


rev :: Int -> Int
rev num = helper num 0
 where
     helper left res
      | left == 0 = res
      | otherwise = helper (div left 10) (res * 10 + mod left 10)