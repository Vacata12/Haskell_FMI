main :: IO()
main = do
    print $ printInterval 100 1000
    print $ allEven 100 1000

printInterval :: Int -> Int -> [Int]
printInterval num1 num2 = [x | x <- [min num1 num2 .. max num1 num2]]

allEven :: Int -> Int -> [Int]
allEven num1 num2 = [x | x <- [min num1 num2 .. max num1 num2], even x]