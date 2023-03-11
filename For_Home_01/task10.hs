main :: IO()
main = do
    print $ finalGrade 3 4 4 4.25 4.50 3.75 4.25 4.25 3.75 == 4.03
    print $ finalGrade 6 6 6 4.50 5 4.50 4.75 5 4.75    == 4.95
    print $ finalGrade 6 0 4 6 6 5 4.75 6 4.75          == 5.14
    print $ finalGrade 4.25 0 3 2 0 0 0 0 0             == 2
    print $ finalGrade 5.50 6 6 6 5.50 5.25 4 5.50 4    == 5.05
    print $ finalGrade 6 6 6 5.50 5.50 4 5 5.50 5       == 5.25
    print $ finalGrade 6 6 6 5.25 6 4 4 5.63 3.50       == 4.84 
    
finalGrade :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
finalGrade d1 d2 d3 kt1 kt2 kz1 kz2 it iz = fromIntegral (round $ result' * 100) / 100
  where 
    tk = (1/4 * ((d1 + d2 + d3)/3)) + (3/8 * ((kt1 + kt2)/2)) + (3/8 * ((kz1 + kz2)/2))
    result = ((0.5 * tk) + (0.25 * it) + (0.25 * iz))
    result' = max result 2
