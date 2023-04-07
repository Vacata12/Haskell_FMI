main :: IO()
main = do
    print $ p 1 == 1
    print $ p 2 == 5
    print $ p 3 == 12
    print $ p 4 == 22
    print $ p 5 == 35
    print $ p 6 == 51

p :: Int -> Int
p n = helper n 0 1
 where
     helper 0 result _ = result
     helper x result up = helper (x - 1) (result + up) (up + 3)