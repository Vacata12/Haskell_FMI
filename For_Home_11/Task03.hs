main :: IO()
main = do
    print $ ordered t1 == True
    print $ ordered t2 == False

type Interval = (Int, Int)
data BTree = Empty | Node Interval BTree BTree
 deriving (Show, Eq)

ordered :: BTree -> Bool
ordered Empty = True
ordered (Node root left right) = helper root left right
 where
  helper :: Interval -> BTree -> BTree -> Bool
  helper _ Empty Empty = True
  helper (x, y) (Node (a, b) _ _) Empty = x < a && a < b && b < y
  helper (x, y) Empty (Node (c, d) _ _) = x < c && c < d && d < y
  helper (x, y) (Node (a, b) _ _) (Node (c, d) _ _) = x < a && a < b && b < y && y < c && c < d
  helper _ _ _ = False