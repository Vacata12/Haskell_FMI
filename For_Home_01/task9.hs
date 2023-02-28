rev :: Int -> Int
rev n
 |n == 0 = 0
 |n == 1 = 1
 |n == 2 = 2
 |n == 3 = 3
 |n == 4 = 4
 |n == 5 = 5
 |n == 6 = 6
 |n == 7 = 7
 |n == 8 = 8 
 |n == 9 = 9
 |otherwise = mod n 10 * 10^((countDigit n) - 1) + rev (div n 10)

countDigit :: Int -> Int
countDigit 0 = 0
countDigit n = 1 + countDigit(div n 10) 

main :: IO()
main = do
    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789