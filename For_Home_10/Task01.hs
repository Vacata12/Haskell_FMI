main :: IO()
main = do
    print $ (sumExpr (2+) [0, 1, 2, 3]) 2 -- == 80
    print $ (sumExpr (*0.8) [0, 1, 2, 3, 4, 5]) 10 -- == 4345680.0

putIndex :: [Int] -> [(Int, Int)]
putIndex ys = zip ys [1 ..]

sumExpr :: (Num a) => (a -> a) -> [Int] -> (a -> a)
sumExpr f ys = \x -> sum [fromIntegral (fst y) * f (x ^ snd y) | y <- putIndex ys]


