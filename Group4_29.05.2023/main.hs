import Data.List
import Data.Char
-- Exam Prep

--Task01
-- isPrime :: Int -> Bool
-- isPrime n = [m | m <- [2 .. (n - 1)], rem n m == 0] == []

-- task01 :: [Int] -> Bool
-- task01 xs = helper 

--Task02

-- task02 :: [(Int -> Int)] -> [Int] -> [Int]
-- task02 [] _ = []
-- task02 (f:fs) xs = nub (filter (\ x -> (f x) > 0) xs ++ task02 fs xs)

-- Task03
canReact :: Char -> Char -> Bool
canReact ch1 ch2 = ch1 /= ch2 && toLower ch1 == toLower ch2

task03 :: [Char] -> Int
task03 str = sum (foldr (canReact ))