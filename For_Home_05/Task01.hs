main :: IO()
main = do
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]

getPrimesLC :: Int -> Int -> [Int]
getPrimesLC num1 num2 = [x | x <- [min num1 num2 .. max num1 num2], isPrime x, containsSeven x]

getPrimesHOF :: Int -> Int -> [Int]
getPrimesHOF num1 num2 = filter isPrime [x | x <- [min num1 num2 .. max num1 num2], containsSeven x]

containsSeven :: Int -> Bool
containsSeven 0 = False
containsSeven num
 | mod num 10 == 7 = True
 | otherwise = containsSeven (div num 10)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = helper n 3
 where
     helper num d
      | d == num = True
      | mod num d == 0 = False
      | otherwise = helper num (d + 1)