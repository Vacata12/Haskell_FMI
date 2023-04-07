main :: IO()
main = do
    print $ calcSeriesSum 1 0 == -2.0 -- x = 1, n = 0
    print $ calcSeriesSum 1 1 == -0.6666666666666667
    print $ calcSeriesSum 1 2 == -1.2000000000000002
    print $ calcSeriesSum 1 3 == -1.047619047619048
    print $ calcSeriesSum 1 4 == -1.0814814814814817
    print $ calcSeriesSum 1 5 == -1.0753246753246755
    print $ calcSeriesSum 1 6 == -1.0762718762718764


calcSeriesSum :: Int -> Int -> Double
calcSeriesSum x n = helper 1 1 3
 where
     helper :: Int -> Int -> Int -> Double
     helper power denom denomMult
      | power > (n + 1) = 0
      | otherwise = (fromIntegral $ (-2) ^ power * x^power) / fromIntegral denom + helper (power + 1) (denom * denomMult) (denomMult + 2)