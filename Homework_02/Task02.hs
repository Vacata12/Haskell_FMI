import Data.List
main :: IO()
main = do
    print $ (getStudios db) [2001]  == [("USA Entertainm.","Stephen Spielberg"),("Buzzfeed Entertainm.","Christian Baesler")]
    print $ (getStudios db) [2002]  == [("Buzzfeed Entertainm.","Christian Baesler")]
    print $ (getStudios db) [1990]  == [("Disney","Merv Griffin"),("Buzzfeed Entertainm.","Christian Baesler")]
    print $ (getStudios db) [1990, 2001, 1976]  == [("Disney","Merv Griffin"),("USA Entertainm.","Stephen Spielberg"),("Buzzfeed Entertainm.","Christian Baesler")]
    print $ (getStudios db) [1979, 2002]  == [("Buzzfeed Entertainm.","Christian Baesler")]

getStudioWithNoMovies :: MovieDB -> [StudioName]
getStudioWithNoMovies (movies, studios, movieExecs) = [name | (name, _) <- studios, not $ any (\(_, _, _, _, studio) -> studio == name) movies]

getStudioWithOneMovie :: MovieDB -> [(StudioName, Year)]
getStudioWithOneMovie (movies, studios, movieExecs) = nub [(studioName, year) | (studioName, _) <- studios, (_, year, _,_,studioName') <- movies, studioName == studioName', length (filter (\(_, _, len, _, studioName'') -> studioName == studioName'' && len > 0) movies) == 1]

getStudios :: MovieDB -> ([Year] -> [(StudioName, Name)])
getStudios (movies, studios, movieExecs) = \ years -> [(studioName, name)| (studioName, producerId) <- studios, (name, producerId', _) <- movieExecs, (elem studioName names && elem (getYear studioName (getStudioWithOneMovie db)) years) || elem studioName (getStudioWithNoMovies db), producerId == producerId']
 where
      names = [name | (name,_) <- getStudioWithOneMovie db]
      getYear :: StudioName -> [(StudioName, Year)]  -> Year
      getYear _ [] = -1
      getYear stName vec
       | stName == fst (head vec) = snd (head vec)
       | otherwise = getYear stName (tail vec)
     
        
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
studios = [("Disney", 199), ("USA Entertainm.", 222), ("Fox",333), ("Paramount", 123), ("MGM", 555), ("Buzzfeed Entertainm.", 42)]
movieExecs :: [MovieExec]
movieExecs = [("George Lucas", 555, 200000000),("Ted Turner", 333,125000000),("Stephen Spielberg", 222, 100000000),("Merv Griffin",199, 112000000),("Calvin Coolidge", 123, 20000000),("Christian Baesler", 42, 420000)]
movies :: [Movie]
movies = [("Pretty Woman", 1990, 119, 'Y', "Disney"),("The Man Who Wasn't There", 2001, 116, 'Y', "USA Entertainm."),("Logan's run",1976, 120, 'Y', "Fox"),("Star Wars", 1977, -1, 'N', "Fox"),("Star Wars 2", 1977, 238, 'N', "Fox"),("Empire Strikes Back", 1980, 111, 'Y', "Fox"),("Star Trek", 1979, 132, 'Y', "Paramount"),("Star Trek: Nemesis", 2002, 116, 'Y', "Paramount"),("Terms of Endearment", 1983, 132, 'N', "MGM"),("The Usual Suspects", 1995, 106, 'Y', "MGM"),("Gone With the Wind", 1938, 238, 'Y', "MGM"),("Gone With the Wind 2", 1938,238, 'Y', "MGM"),("The Fellowship of the Ring", 2001, -1, 'Y', "USA Entertainm.")]
db :: MovieDB
db = (movies, studios, movieExecs)