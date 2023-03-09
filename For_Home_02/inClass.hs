isPolindron :: Int -> Bool
isPolindron n = rev n == n

rev :: Int -> Int
rev n = helper n 0
 where
    helper 0 result = result
    