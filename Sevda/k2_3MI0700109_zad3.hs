import Data.List
main :: IO()
main = do
   print $ mapLevel t1 [(*2), (*4), (`div` 100)]  == t11Result
   print $ mapLevel t1 [show, (nub . show . (* 1000))] == t12Result
   print $ mapLevel t2 [(\ _ -> "r"), (\ char -> "w_" ++ [char]), (\ char -> "l_" ++ [char])] == t2Result 

t1 = Node 10 [Node 10 [Node 10 [], Node 8 [Node 10 []], Node 2 []], Node 10 [Node 11 [], Node 10 [], Node 6 []]]
t11Result = Node 20 [Node 40 [Node 0 [],Node 0 [],Node 0 []],Node 40 [Node 0 [],Node 0 [],Node 0 []]]
t2 = Node 's' [Node 's' [Node 's' [], Node 's' []]]


t12Result = Node "10" [Node "10" [],Node "10" []]
t2Result = Node "r" [Node "w_s" [Node "l_s" [],Node "l_s" []]]

data NTree a = Nil | Node a [NTree a] deriving (Eq, Show)

mapLevel :: NTree a -> [(a -> b)] -> NTree b
mapLevel Nil _ = Nil
mapLevel (Node x children) functions = Node (applyFunctions functions x) (mapLevelChildren children (tail functions))
  where
    applyFunctions :: [(a -> b)] -> a -> b
    applyFunctions [] _ = error "Not enough functions provided"
    applyFunctions (f:_) a = f a

    mapLevelChildren :: [NTree a] -> [(a -> b)] -> [NTree b]
    mapLevelChildren [] _ = []
    mapLevelChildren _ [] = []
    mapLevelChildren (c:cs) fs = mapLevel c fs : mapLevelChildren cs fs


