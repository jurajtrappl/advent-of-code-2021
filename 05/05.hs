import Data.List (group, sort)
import Data.List.Split (splitOn)

data Point = Point Int Int deriving (Eq, Show)
data Line = Line Point Point deriving Show

instance Ord Point where
    (Point x1 y1) <= (Point x2 y2) = x1 < x2 || (x1 == x2) && (y1 < y2)

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

toPoint :: String -> Point
toPoint s = Point (unsafeReadInt $ head splitted) (unsafeReadInt $ last splitted)
    where splitted = splitOn "," s

toLine :: [Point] -> Line
toLine pts = Line (minimum pts) (maximum pts)

parseInput :: IO [Line]
parseInput = do
    fContent <- readFile "05.in"
    return $ map (toLine . map toPoint . splitOn " -> ") $ lines fContent

isDiagonal :: Line -> Bool
isDiagonal (Line (Point x1 y1) (Point x2 y2)) = abs (x1 - x2) == abs (y1 - y2)

getDiagonalPoints :: Line -> [Point]
getDiagonalPoints (Line start@(Point x1 y1) end@(Point x2 y2))
    | x1 == y1 && x2 == y2 = zipWith Point [x1..x2] [y1..y2]
    | x1 < x2 && y1 > y2 = zipWith Point [x1..x2] (reverse [y2..y1])
    | x1 < x2 && y1 < y2 = zipWith Point [x1..x2] [y1..y2]
    | otherwise = zipWith Point [x2..x1] [y1..y2]

getPoints :: Line -> [Point] -- diagonal, horizontal and vertical
getPoints l@(Line (Point x1 y1) (Point x2 y2))
    | isDiagonal l = getDiagonalPoints l
    | x1 == x2 = [Point x1 y | y <- [y1..y2]]
    | otherwise = [Point x y1 | x <- [x1..x2]]

atleastTwoIntersections :: [Point] -> Int
atleastTwoIntersections = length . filter (>= 2) . map length . group . sort

fstPart :: IO ()
fstPart = do
    ls <- parseInput
    let points = concatMap getPoints $ filter (not . isDiagonal) ls
    print $ atleastTwoIntersections points

sndPart :: IO ()
sndPart = do
    ls <- parseInput
    let points = concatMap getPoints ls
    print $ atleastTwoIntersections points