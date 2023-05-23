main :: IO()
main = do
    print $ isGraceful t1 == True
    print $ isGraceful t2 == False

data NTree a = T a [(NTree a)]
 deriving (Show, Eq)

t1 = T 1 [T 3 [], T 5 [], T 7 [], T 9 []]
t2 = T 7 [T 9 [T 5 [], T 2 []]]

getList :: NTree a -> [a]
getList (T r xs) = r : concatMap getList xs

isGraceful ::(Num a) => NTree a -> Bool
isGraceful tree = helper (getList tree) (abs (head (head (getList tree)) - head (getList tree)))
 where
     helper (x:xs) dif
      | abs(x - head xs) /= difference = False


