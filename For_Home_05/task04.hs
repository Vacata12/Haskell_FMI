main :: IO()
main = do
    print $ specialSum 1 100 == 195 -- 61, 65, 69

specialSum :: Int -> Int -> Int
specialSum x y = sum [d | d <- [x .. y], mod d 4 == 1 && elem '6' (show d)]