maxMultiple :: Int -> Int -> Int
maxMultiple d b = helper d b b
 where
    helper d b result
     | mod result d == 0 && result <= b && result > 0 = result
     | otherwise = helper d b (result - 1)

main :: IO()
main = do
   print $ maxMultiple 2 7 == 6
   print $ maxMultiple 3 10 == 9
   print $ maxMultiple 7 17 == 14
   print $ maxMultiple 10 50 == 50
   print $ maxMultiple 37 200 == 185
   print $ maxMultiple 7 100 == 98  
   print $ maxMultiple 7 10 == 7
   print $ maxMultiple 4 4 == 4