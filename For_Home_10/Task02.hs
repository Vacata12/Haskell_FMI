main :: IO()
main = do
   print $ coldestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"


type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]

coldestCapital :: [Country] -> Name
coldestCapital countries = countryName $ foldl1 (\x y -> if avgTemp (cities x) <= avgTemp (cities y) then x else y) countries
  where 
        countryName (Country name _ _) = name
        cities (Country _ _ cities) = cities

avgTemp :: [City] -> Double
avgTemp cities = sum [x | (City _ _ x) <- cities] / fromIntegral (length cities)
