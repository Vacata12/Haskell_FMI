main :: IO()
main = do
    print $ minDepthGreenNode colorTree == 2

data Color = Red | Green | Blue
data Tree = Empty | Node Color Tree Tree

colorTree = 