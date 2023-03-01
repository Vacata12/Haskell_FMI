finalGrade :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
finalGrade d1 d2 d3 kz1 kz2 kt1 kt2 iz it = sumTk (sumHw d1 d2 d3) (sumKz kz1 kz2) (sumKt kt1 kt2) + it / 4 + iz / 4

sumHw :: Double -> Double -> Double -> Double
sumHw d1 d2 d3 = (d1 + d2 + d3) / 4

sumKz :: Double -> Double -> Double
sumKz kz1 kz2 = (kz1 + kz2) * 3 / 8

sumKt :: Double -> Double -> Double
sumKt kt1 kt2 = (kt1 + kt2) * 3 / 8

sumTk :: Double -> Double -> Double -> Double
sumTk hw kz kt = (hw + kz + kt) / 2

main :: IO()
main = do
    print $ finalGrade 3 4 4 4.25 4.50 3.75 4.25 5 4.25 == 4.34
    print $ finalGrade 6 6 6 4.50 5 4.50 4.75 5 4.75    == 4.95
    print $ finalGrade 6 0 4 6 6 5 4.75 6 4.75          == 5.14
    print $ finalGrade 4.25 0 3 2 0 0 0 0 0             == 2
    print $ finalGrade 5.50 6 6 6 5.50 5.25 4 5.50 4    == 5.05
    print $ finalGrade 6 6 6 5.50 5.50 4 5 5.50 5       == 5.25
    print $ finalGrade 6 6 6 5.25 6 4 4 5.63 3.50       == 4.84