import Data.List
main :: IO()
main = do
    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587]
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30]  == area (Cylinder 20.0 30.0)

data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a

area :: (Floating a) => Shape a -> a
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b
area (Triangle a b c) = sqrt(((a + b + c) / 2) * (((a + b + c) / 2) - a) * (((a + b + c) / 2) - b) * (((a + b + c) / 2) - c))
area (Cylinder r h) = 2* pi * r * h + 2 * pi * r * r

getAreas :: (Floating a) => [Shape a] -> [a]
getAreas [] = []
getAreas gs = map area gs

maxArea :: (Floating a, Ord a) => [Shape a] -> a
maxArea shapes = foldl (\acc shape -> if area shape > acc then area shape else acc) 0 shapes

