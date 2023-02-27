sqAvg :: Double -> Double -> Double
sqAvg n m = (n ^ 2 + m ^ 2) / 2

main :: IO()
main = do
    print $ sqAvg 5 0 == 12.5
    print $ sqAvg 10 13 == 134.5