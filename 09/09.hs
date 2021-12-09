{-# LANGUAGE RecordWildCards #-}

import qualified Data.List as List
import qualified Data.Matrix as Matrix
import Data.Maybe (isJust)

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
getNeighbours heightmap center@(Pos { .. }) = filter (validPosition heightmap) positionsAround
    where positionsAround = [Pos (row - 1) col, Pos row (col - 1), Pos row (col + 1), Pos (row + 1) col]

valueFromPos :: HeightMap -> Position -> Int
valueFromPos heightMap (Pos{ .. }) = heightMap Matrix.! (row, col)

areSmaller :: HeightMap -> Position -> Bool
areSmaller heightMap p@(Pos { .. }) = all (> heightMap Matrix.! (row, col)) neighboursValues
    where neighbours = getNeighbours heightMap p
          neighboursValues = map (valueFromPos heightMap) neighbours

findSmaller :: HeightMap -> [Position] -> [Int]
findSmaller heightMap = map (\(_, p) -> valueFromPos heightMap p) . filter fst . map (\p -> (areSmaller heightMap p, p))

toRiskLevels :: [Int] -> [Int]
toRiskLevels = map (+1)

fstPart :: IO ()
fstPart = do
    input <- parseInput
    let heightMap = Matrix.fromLists input
    let lowPoints = findSmaller heightMap [Pos r c | r <- [1..Matrix.nrows heightMap], c <- [1..Matrix.ncols heightMap]]
    print $ sum $ toRiskLevels lowPoints