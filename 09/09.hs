{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Graph as Graph
import qualified Data.Matrix as Matrix
import Data.Maybe (isJust)
import Data.List (foldl', sort)

type HeightMap = Matrix.Matrix Int

data Position = Pos
  { row :: Int
  , col :: Int
  } deriving (Eq, Show)

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

parseInput :: IO [[Int]]
parseInput = fmap (map (map (unsafeReadInt . (:[]))) . lines) (readFile "09.in")

validPosition :: HeightMap -> Position -> Bool
validPosition heightMap (Pos { .. }) = isJust $ Matrix.safeGet row col heightMap

getNeighbours :: HeightMap -> Position -> [Position]   -- up, left, right, down
getNeighbours heightmap (Pos { .. }) = filter (validPosition heightmap) positionsAround
    where positionsAround = [Pos (row - 1) col, Pos row (col - 1), Pos row (col + 1), Pos (row + 1) col]

valueFromPos :: HeightMap -> Position -> Int
valueFromPos heightMap (Pos{ .. }) = heightMap Matrix.! (row, col)

areSmaller :: HeightMap -> Position -> Bool
areSmaller heightMap p@(Pos { .. }) = let neighboursValues = map (valueFromPos heightMap) $ getNeighbours heightMap p
                                          value = heightMap Matrix.! (row, col)
                                      in all (> value) neighboursValues

findSmaller :: HeightMap -> [Position] -> [Position]
findSmaller heightMap = map snd . filter fst . map (\p -> (areSmaller heightMap p, p))

toRiskLevels :: [Int] -> [Int]
toRiskLevels = map (+1)

allPositions :: HeightMap -> [Position]
allPositions heightMap = [Pos r c | r <- [1..Matrix.nrows heightMap], c <- [1..Matrix.ncols heightMap]]

fstPart :: IO ()
fstPart = do
    input <- parseInput
    let heightMap = Matrix.fromLists input
    let lowPoints = findSmaller heightMap (allPositions heightMap)
    print $ sum $ toRiskLevels $ map (valueFromPos heightMap) lowPoints

searchForId :: [(Position, Int)] -> Position -> Int
searchForId idsWithPos pos = snd $ head $ filter (\(p, id) -> p == pos) idsWithPos

connectToNeighbours :: HeightMap -> [(Position, Int)] -> Position -> [Graph.Edge]
connectToNeighbours heightMap positionsIds pos = map (searchForId positionsIds pos,) neighbourIds
    where validNeighbours = filter (\(Pos { .. }) -> heightMap Matrix.! (row, col) /= 9) $ getNeighbours heightMap pos
          neighbourIds = map (searchForId positionsIds) validNeighbours

sndPart :: IO ()
sndPart = do
    input <- parseInput
    let heightMap = Matrix.fromLists input
    let positions = allPositions heightMap
    let positionsIds = zip positions [1..length positions]
    let edges = concatMap (connectToNeighbours heightMap positionsIds) positions
    let graph = Graph.buildG (1, snd $ last positionsIds) edges
    let lowPointsIds = map (searchForId positionsIds) $ findSmaller heightMap positions
    print $ foldl' (*) 1 $ take 3 $ foldl' (flip (:)) [] $ sort $ map (length . Graph.reachable graph) lowPointsIds