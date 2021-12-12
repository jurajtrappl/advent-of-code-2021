{-# LANGUAGE RecordWildCards #-}

import qualified Data.Matrix as Matrix
import Data.Maybe (isJust)
import Data.Functor ((<&>))

type Octopuses = Matrix.Matrix Int

data Position = Pos
  { row :: Int
  , col :: Int
  } deriving (Eq, Show)

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

parseInput :: IO Octopuses
parseInput = fmap (map (map (unsafeReadInt . (:[]))) . lines) (readFile "11.in") <&> Matrix.fromLists

validPosition :: Octopuses -> Position -> Bool
validPosition heightMap (Pos { .. }) = isJust $ Matrix.safeGet row col heightMap

getNeighbours :: Octopuses -> Position -> [Position]
getNeighbours heightmap center@(Pos { .. }) =
    filter (\p -> validPosition heightmap p && p /= center) positionsAround
    where positionsAround = [Pos r c | r <- [row - 1..row + 1], c <- [col - 1..col + 1]]

allIncreaseEnergy :: Octopuses -> Octopuses
allIncreaseEnergy = fmap (+1)

updateElem :: Octopuses -> Position -> Int -> Octopuses
updateElem energies (Pos { .. }) value = Matrix.setElem value (row, col) energies

increaseEnergy :: Octopuses -> Position -> Octopuses
increaseEnergy energies p@(Pos { .. }) = updateElem energies p (energies Matrix.! (row, col) + 1)

resetAfterFlash :: Octopuses -> Position -> Octopuses
resetAfterFlash energies p@(Pos { .. }) = updateElem energies p 0

willFlash :: Octopuses -> [Position] -> [Position]
willFlash energies = filter (\(Pos { .. }) -> energies Matrix.! (row, col) >= 10)

allPositions :: Octopuses -> [Position]
allPositions energies = [Pos r c | r <- [1..Matrix.nrows energies], c <- [1..Matrix.ncols energies]]

propagate :: Octopuses -> Octopuses
propagate energies
  | not $ null readyToFlash = propagate $ flash energies (head readyToFlash)
  | otherwise = energies
  where readyToFlash = willFlash energies (allPositions energies)

isFlashed :: Octopuses -> Position -> Bool
isFlashed energies (Pos { .. }) = energies Matrix.! (row, col) == 0

flash :: Octopuses -> Position -> Octopuses
flash energies toFlash = foldl increaseEnergy currentToZero neighbours
  where currentToZero = resetAfterFlash energies toFlash
        neighbours = filter (not . isFlashed currentToZero) $ getNeighbours currentToZero toFlash

countFlashed :: Octopuses -> Int
countFlashed energies = length (filter (isFlashed energies) (allPositions energies))

step :: Int -> Octopuses -> Int
step 0 energies = 0
step count energies = countFlashed propagated + step (count - 1) propagated
  where propagated = propagate $ allIncreaseEnergy energies

fstPart :: IO ()
fstPart = parseInput >>= print . step 100

untilAllFlash :: Octopuses -> Int
untilAllFlash energies
  | Matrix.zero (Matrix.nrows energies) (Matrix.ncols energies) == energies = 0
  | otherwise = 1 + untilAllFlash (propagate $ allIncreaseEnergy energies)

sndPart :: IO ()
sndPart = parseInput >>= print . untilAllFlash