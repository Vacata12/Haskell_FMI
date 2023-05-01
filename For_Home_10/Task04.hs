import Data.List
main :: IO()
main = do
    print $ findUncles t 5 == [3,4]
    print $ findUncles t 7 == [2,4]
    print $ findUncles t 10 == [5]

type Tree = [(Int, [Int])]

t :: Tree
t = [(1,[2,3,4]),(2,[5,6]),(3,[7]),(4,[8,9]),(5,[]),(6,[10]),(7,[]),(8,[]),(9,[]),(10,[])]

findUncles :: Tree -> Int -> [Int]
findUncles tree x = [fst node | node <- tree, isUncle node x]

isUncle :: (Int, [Int]) -> Int -> Bool
isUncle (node, children) x = any (\child -> x `elem` snd (tree !! (child - 1))) children
  where tree = sortOn fst tree