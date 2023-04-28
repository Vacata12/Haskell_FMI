import Data.List
type Node = Int
type Graph = [(Node, [Node])]
type Path = [Node]

main :: IO()
main = do
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1  == [[1]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1  == [[1, 2], [1, 3]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1  == [[1, 2, 3], [1, 2, 4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2  == [[2,3],[2,4]]


getChildren :: Graph -> Node -> [Node]
getChildren g n = head [children | (parent, children) <- g, parent == n]

isPath :: Graph -> Path -> Bool
isPath g path = all (\ (parent, child) -> elem child (getChildren g parent))
                    $ zip path (tail path)

simplePaths :: Graph -> Int -> Node -> [Path]
simplePaths gr k n = [xs | xs <- subsequences (map fst gr), isPath gr xs, length xs == k + 1, head xs == n]
