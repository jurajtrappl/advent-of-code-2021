{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (foldl')

type Range = (Int, Int)
type CubesRanges = (Range, Range, Range)
type Location = (Int, Int, Int)
type Reactor = Map.Map Location Bool

data RebootStep = RebootStep
                { command :: Bool
                , ranges :: CubesRanges
                } deriving (Show)

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

parseRange :: String -> Range
parseRange unproc = let splitted = splitOn ".." unproc
                    in bimap unsafeReadInt unsafeReadInt (head splitted, last splitted)

commandToBool :: String -> Bool
commandToBool "on" = True
commandToBool "off" = False

tuplify3 :: [a] -> (a, a, a)
tuplify3 [a, b, c] = (a, b, c)

parseRebootStep :: String -> RebootStep
parseRebootStep s = RebootStep command (tuplify3 $ map parseRange $ concat ranges)
    where command = commandToBool $ head $ words s
          dimensions = splitOn "," s
          ranges = map (take 1 . drop 1 . splitOn "=") dimensions

parseInput :: IO [RebootStep]
parseInput = map parseRebootStep . lines <$> readFile "22.in"

cuboid :: CubesRanges -> [Location]
cuboid ((x, x'), (y, y'), (z, z')) = [(a, b, c) | a <- [x..x'], b <- [y..y'], c <- [z..z']]

updateReactor :: Reactor -> Bool -> Location -> Reactor
updateReactor reactor command location = Map.insert location command reactor
    where deleted = Map.delete location reactor

performStep :: Reactor -> RebootStep -> Reactor
performStep reactor (RebootStep { .. }) = foldl' (`updateReactor` command) reactor cubesLocations
    where cubesLocations = cuboid ranges

rebootReactor :: [RebootStep] -> Reactor
rebootReactor = foldl' performStep reactor
    where reactor = Map.fromList $ zip cubeIndices $ replicate (length cubeIndices) False
          cubeIndices = cuboid ((-50, 50), (-50, 50), (-50, 50))

inRange :: CubesRanges -> Bool
inRange ((x, x'), (y, y'), (z, z')) = (x >= -50 && x' <= 50) || (y >= -50 && y' <= 50) || (z >= -50 && z' <= 50)

fstPart :: IO ()
fstPart = do
    steps <- parseInput
    let prunedSteps = filter (\(RebootStep { .. }) -> inRange ranges) steps
    let rebooted = rebootReactor prunedSteps
    print $ length $ filter snd $ Map.toList rebooted

sndPart :: IO ()
sndPart = parseInput >>= print . length . filter snd . Map.toList . rebootReactor