main :: IO()
main = do
    print $ sumUnevenLC 5 50 == 621
    print $ sumUnevenLC 50 1 == 625
    print $ sumUnevenLC 564 565 == 565

    print $ sumUnevenHOF 5 50 == 621
    print $ sumUnevenHOF 50 1 == 625
    print $ sumUnevenHOF 564 565 == 565

sumUnevenLC :: Int -> Int -> Int
sumUnevenLC num1 num2 = sum [x | x <- [min num1 num2 .. max num1 num2], not $ even x]

sumUnevenHOF :: Int -> Int -> Int
sumUnevenHOF num1 num2 = sum $ filter notEven [x | x <- [min num1 num2 .. max num1 num2]]

notEven :: Int -> Bool
notEven num =  not $ even num