main :: IO()
main = do
    print $ (sumExpr (2+) [0, 1, 2, 3]) 2 == 80
    print $ (sumExpr (*0.8) [0, 1, 2, 3, 4, 5]) 10 == 4345680.0

sumExpr :: (Num a) => (a -> a) -> [Int] -> (a -> a)
sumExpr f xs = (\x -> sum [ fromIntegral (y) * f (x^i) | (y, i) <- zip xs [1 ..]])


