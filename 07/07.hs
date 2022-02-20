import Data.List.Split (splitOn)
import Data.List (sort)

type HorizontalPos = Int

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

parseInput :: IO [HorizontalPos]
parseInput = fmap (map unsafeReadInt . splitOn ",") (readFile "07.in")

median :: [HorizontalPos] -> Int
median pos
    | even n = div (pos !! div n 2 + pos !! div (n + 1) 2) 2
    | otherwise = pos !! div (n + 1) 2
    where n = length pos

countConstantBurnRateFuelCost :: HorizontalPos -> [HorizontalPos] -> Int
countConstantBurnRateFuelCost bestPos = foldl (\acc p -> acc + abs (p - bestPos)) 0

fstPart :: IO ()
fstPart = parseInput >>= (\positions -> print $ countConstantBurnRateFuelCost (median $ sort positions) positions)

arithMean :: [HorizontalPos] -> Int
arithMean pos = floor (fromIntegral (sum pos) / fromIntegral (length pos))

countIncreasingBurnRateFuelCost :: HorizontalPos -> [HorizontalPos] -> Int
countIncreasingBurnRateFuelCost bestPos = foldl (\acc p -> acc + sum (take (abs (bestPos - p)) [1..])) 0

sndPart :: IO ()
sndPart = parseInput >>= (\positions -> print $ countIncreasingBurnRateFuelCost (arithMean positions) positions)