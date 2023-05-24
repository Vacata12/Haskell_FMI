import Data.List
main :: IO()
main = do
    print $ isGraceful t1 == True
    print $ isGraceful t2 == False

data NTree a = T a [(NTree a)]
 deriving (Show, Eq)

t1 = T 1 [T 3 [], T 5 [], T 7 [], T 9 []]
t2 = T 7 [T 9 [T 5 [], T 2 []]]

getList ::(Num a) => NTree a -> [a]
getList (T r xs) = r : concatMap getList xs

isGraceful ::(Num a, Eq a, Ord a) => NTree a -> Bool
isGraceful t = isGraceful' (sort (getList t)) ( head (tail (sort (getList t))) - (head (sort (getList t))) )
 where
  isGraceful' [] _ = True
  isGraceful' [x] _ = True
  isGraceful' (x:y:xs) dif = (y - x == dif) && isGraceful' xs dif


         


