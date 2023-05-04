getNthSevenlikeNum :: Int -> Int
getNthSevenlikeNum 1 = 1 
getNthSevenlikeNum n 
 | n <= 0 = error "Negative num! Num must be positive" 
 | mod n 2 == 1 = 1 + 7 * getNthSevenlikeNum(div n 2) 
 | otherwise = 7 * getNthSevenlikeNum(div n 2)