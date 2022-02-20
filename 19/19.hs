import Data.List.Split (splitOn)

type Location = (Int, Int)
type Beacons = [Location]
type Scanner = (Location, Beacons)

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)

parseLocation :: String -> Location
parseLocation = tuplify . map unsafeReadInt . splitOn ","

parseInput = fmap (map (map parseLocation . drop 1 . lines) . splitOn "\n\n") (readFile "19.in")