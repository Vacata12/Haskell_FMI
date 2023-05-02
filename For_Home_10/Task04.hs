import Data.List
main :: IO()
main = do
    print $ findUncles t 5 == [3,4]
    print $ findUncles t 7 == [2,4]
    print $ findUncles t 10 == [5]

type Tree = [(Int, [Int])]

t :: Tree
t = [(1,[2,3,4]),(2,[5,6]),(3,[7]),(4,[8,9]),(5,[]),(6,[10]),(7,[]),(8,[]),(9,[]),(10,[])]

getFather :: Tree -> Int -> Int
getFather tree node = head [x | (x, y) <- tree, elem node y, length y > 0]

getBrothers :: Tree -> Int -> [Int]
getBrothers [] _ = []
getBrothers tree node = helper tree node
 where
     helper [] _ = []
     helper copyTree node
      | elem (getFather tree node) (snd (head copyTree)) = snd $ head copyTree
      | otherwise = helper (tail copyTree) node

findUncles :: Tree -> Int -> [Int]
findUncles tree node = filter (/= getFather tree node) [z | z <- y]
 where
     y = getBrothers tree node






