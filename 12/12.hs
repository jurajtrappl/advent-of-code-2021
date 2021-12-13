import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Data.List (sort, group)
import Data.Char (isLower)

type Vertex = String
type Edge = (Vertex, Vertex)
type Graph = [(Vertex, [Vertex])]
type Path = [Edge]

type Queue = [(Vertex, [Vertex])]
type Queue' = [(Vertex, [Vertex], Bool)]

toEdge :: String -> Edge
toEdge s = let splitted = splitOn "-" s
           in (head splitted, last splitted)

unique :: (Eq a, Ord a) => [(a, a)] -> [a]
unique tuples = map head $ group $ sort $ map fst tuples ++ map snd tuples

findNeighbours :: [Edge] -> Vertex -> (Vertex, [Vertex])
findNeighbours edges v = (v, filter (/= v) $ unique isVertexPresent)
    where isVertexPresent = filter (\(v1, v2) -> v == v1 || v == v2) edges

makeGraph :: [Edge] -> Graph
makeGraph edges = map (findNeighbours edges) (unique edges)

parseInput :: IO Graph
parseInput = fmap (map toEdge . lines) (readFile "12.in") <&> makeGraph

fstPartPaths :: Graph -> Int
fstPartPaths = fstPartSearch [("start", ["start"])]

isLowercase :: Vertex -> Bool
isLowercase = all isLower

fstPartCreateSmalls :: [Vertex] -> [Vertex] -> [(Vertex, [Vertex])]
fstPartCreateSmalls [] _ = []
fstPartCreateSmalls (v:vs) currentSmall
    | isLowercase v = (v, currentSmall ++ [v]) : fstPartCreateSmalls vs currentSmall
    | otherwise = (v, currentSmall) : fstPartCreateSmalls vs currentSmall

fstPartSearch :: Queue -> Graph -> Int
fstPartSearch [] _ = 0
fstPartSearch ((current, small):qs) g
    | current == "end" = 1 + fstPartSearch qs g
    | otherwise = fstPartSearch (qs ++ fstPartCreateSmalls notSmall small) g
    where neighbours = snd $ head $ filter (\(v, _) -> v == current) g
          notSmall = filter (`notElem` small) neighbours

fstPart :: IO ()
fstPart = parseInput >>= print . fstPartPaths
          
sndPartPaths :: Graph -> Int
sndPartPaths = sndPartSearch [("start", ["start"], False)]

sndPartCreateSmalls :: [Vertex] -> [Vertex] -> Bool -> [(Vertex, [Vertex], Bool)]
sndPartCreateSmalls [] _ _ = []
sndPartCreateSmalls (v:vs) currentSmall twice
    | isLowercase v = (v, currentSmall ++ [v], twice) : sndPartCreateSmalls vs currentSmall twice
    | otherwise = (v, currentSmall, twice) : sndPartCreateSmalls vs currentSmall twice

sndPartTwiceSmall :: [Vertex] -> [Vertex] -> [(Vertex, [Vertex], Bool)]
sndPartTwiceSmall [] _ = []
sndPartTwiceSmall (v:vs) small
    | v /= "start" && v /= "end" = (v, small, True) : sndPartTwiceSmall vs small
    | otherwise = sndPartTwiceSmall vs small

sndPartSearch :: Queue' -> Graph -> Int
sndPartSearch [] _ = 0
sndPartSearch ((current, small, twice):qs) g
    | current == "end" = 1 + sndPartSearch qs g
    | otherwise = sndPartSearch (qs ++ newQueue ++ twiceQueue) g
    where neighbours = snd $ head $ filter (\(v, _) -> v == current) g
          notSmall = filter (`notElem` small) neighbours
          newQueue = sndPartCreateSmalls notSmall small twice
          twiceQueue = if twice then [] else sndPartTwiceSmall (filter (`elem` small) neighbours) small

sndPart :: IO ()
sndPart = parseInput >>= print . sndPartPaths