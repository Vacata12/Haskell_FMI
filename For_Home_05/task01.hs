main :: IO()
main = do
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]

isPrime :: Int -> Bool
isPrime n = n > 1 && null [d | d <- [2 .. n - 1], mod n d == 0]

getPrimesLC :: Int -> Int -> [Int]
getPrimesLC a b = [n | n <- [min a b .. max a b], isPrime n, elem '7' (show n)]

getPrimesHOF :: Int -> Int -> [Int]
getPrimesHOF a b = filter (\n -> isPrime n && elem '7' (show n)) [min a b .. max a b]