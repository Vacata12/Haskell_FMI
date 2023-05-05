main :: IO()
main = do
    print $ getNameLengthColor db ((== 'Y'), (> 106))   == [("PrettyWoman",119),("The Man Who Wasn't There",116),("Logan'srun",120),("Empire Strikes Back",111),("Star Trek",132),("Star Trek:Nemesis",116)]
    print $ getNameLengthColor db ((== 'Y'), (> 237))   == []
    print $ getNameLengthColor db ((== 'Y'), (> 238))   == []
    print $ getNameLengthColor db ((== 'N'), (< 133))   == [("Terms ofEndearment",132)]
    print $ getNameLengthColor db ((== 'N'), (< 300))   == [("Terms ofEndearment",132)]


getNameLengthColor :: MovieDB -> ((Char -> Bool), (Int -> Bool)) -> [(Title, Length)]
getNameLengthColor (movies, _, _) (colorB, lengthB) = [(title, length) | (title, _ ,length, color, _) <- movies, colorB color, lengthB length, length > 0, length /= getLongestColorLength db]

getLongestColorLength :: MovieDB -> Length
getLongestColorLength (movies, _, _) = foldl (\maxLen (_, _, len, _, _) -> max maxLen len) 0 colorMovies
 where
     colorMovies = filter (\(_, _, len, color, _) -> color == 'Y') movies


type Title = String
type Year = Int
type Length = Int
type InColor = Char
type StudioName = String
type Name = String
type ProducerID = Int
type Networth = Integer
type Movie = (Title, Year, Length, InColor, StudioName)
type Studio = (Name, ProducerID)
type MovieExec = (Name, ProducerID, Networth)
type MovieDB = ([Movie], [Studio], [MovieExec])

studios :: [Studio]
studios = [("Disney", 199),("USA Entertainm.", 222),("Fox",333),("Paramount", 123),("MGM", 555),("Buzzfeed Entertainm.", 42)]
movieExecs :: [MovieExec]
movieExecs = [("George Lucas", 555, 200000000),("Ted Turner", 333,125000000),("Stephen Spielberg", 222, 100000000),("Merv Griffin",199, 112000000),("Calvin Coolidge", 123, 20000000),("ChristianBaesler", 42, 420000)]
movies :: [Movie]
movies = [("Pretty Woman", 1990, 119, 'Y', "Disney"),("The Man WhoWasn't There", 2001, 116, 'Y', "USA Entertainm."),("Logan's run",1976, 120, 'Y', "Fox"),("Star Wars", 1977, -1, 'N', "Fox"),("Star Wars2", 1977, 238, 'N', "Fox"),("Empire Strikes Back", 1980, 111, 'Y',"Fox"),("Star Trek", 1979, 132, 'Y', "Paramount"),("Star Trek:Nemesis", 2002, 116, 'Y', "Paramount"),("Terms of Endearment", 1983,132, 'N', "MGM"),("The Usual Suspects", 1995, 106, 'Y', "MGM"),("GoneWith the Wind", 1938, 238, 'Y', "MGM"),("Gone With the Wind 2", 1938,238, 'Y', "MGM"),("The Fellowship of the Ring", 2001, -1, 'Y', "USAEntertainm.")]
db :: MovieDB
db = (movies, studios, movieExecs)
