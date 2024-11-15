import Data.Char
main :: IO()
main = do
   print $ solve ["abode","ABc","xyzD"] == [4,3,1]
   print $ solve ["abide","ABc","xyz"] == [4,3,0]
   print $ solve ["IAMDEFANDJKL","thedefgh","xyzDEFghijabc"] == [6,5,7]
   print $ solve ["encode","abc","xyzD","ABmD"] == [1,3,1,3]

solve :: [String] -> [Int]
solve = map matchingPositions

matchingPositions :: String -> Int
matchingPositions word = length $ filter (\(c, letter) -> toLower c == letter) $ zip word alphabet
  where
    alphabet = ['a'..'z']

