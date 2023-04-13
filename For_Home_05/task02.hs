main :: IO()
main = do
    print $ isPalindrom 121 == True
    print $ getPalindromes 132465 == 8
    print $ getPalindromes 654546 == 8
    print $ getPalindromes 100001 == 100012
    print $ getPalindromes 21612 == 21614
    print $ getPalindromes 26362 == 26364

-- getPalindromes :: Int -> Int 
-- getPalindromes n = sum [minimum [k | k <- [2..n], mod n k == 0 && isPalindrom k], maximum [k | k <- [2..n], mod n k == 0 && isPalindrom k]]

getPalindromes :: Int -> Int
getPalindromes n = head paliDivs + last paliDivs
 where
    paliDivs = [d | d <- [2 .. n], isPalindrom d && mod n d == 0]
 
isPalindrom :: Int -> Bool
isPalindrom x = x == rev x

rev :: Int -> Int
rev = read . reverse . show



