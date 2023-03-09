sumDigitsIter :: Int -> Int
sumDigitsIter n = helper n 0
 where
     helper 0 result = result
     helper leftover result = helper (div leftover 10) result + mod leftover 10

main :: IO()
main = do
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6