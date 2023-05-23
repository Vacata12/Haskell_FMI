main :: IO()
main = do
    print $ prune t1 -- == T 1 [T 2 [T 3 []], T 4 [T 5 []], T 7 [T 8 [], T 9 [T 10 []]]]                                                                    
                       --  T 1 [T 2 [T 3 []],T 4 [],T 7 [T 8 [],T 9 []]]
data NTree a = T a [(NTree a)]
 deriving (Show, Eq)
 
t1 = T 1 [T 2 [T 3 []], T 4 [T 5 [T 6 []]], T 7 [T 8 [], T 9 [T 10 [T 11 []]]]]

len :: NTree a -> Int
len (T _ []) = 0
len (T _ xs) = 1 + sum (map len xs)

prune :: (Num a, Eq a) => NTree a -> NTree a
prune (T r xs) = helper (T r xs) r
 where
     helper (T r xs) root
       | len (T r xs) == 7 && r /= root = T r []
       | otherwise =  T r 



-- prune :: (Num a) => NTree a -> NTree a
-- prune (T r xs)
--  | map len xs > 2 = []
--  | otherwise = prune xs



