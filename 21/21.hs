{-# LANGUAGE TupleSections #-}

import Data.List.Split (splitOn)

type TrackPos = Int
type Roll = [Int]
type Score = Int
type Player = (TrackPos, Score)

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

parseInput :: IO [TrackPos]
parseInput = fmap (map (unsafeReadInt . last . splitOn ": ") . lines) (readFile "21.in")

rollDeterministicDice :: Int -> Roll
rollDeterministicDice n = map (`mod` 100) [n + 1, n + 2, n + 3]

nextPosition :: TrackPos -> Int -> TrackPos
nextPosition currentPos lastRoll = if nextPos == 0 then 10 else nextPos
    where nextPos = (`mod` 10) $ currentPos + (`mod` 10) (sum $ rollDeterministicDice lastRoll)

simulate :: Roll -> Int -> [Player] -> Int
simulate prevRoll turnNum [(fstPos, fstScore), (sndPos, sndScore)]
    | fstScore + fstTurn >= 1000 = sndScore * (6 * turnNum + 3)
    | sndScore + sndTurn >= 1000 = (fstScore + fstTurn) * (6 * turnNum + 6)
    | otherwise = do
        simulate [last prevRoll + 6] (turnNum + 1) [(fstTurn, fstScore + fstTurn), (sndTurn, sndScore + sndTurn)]
    where fstTurn = nextPosition fstPos (last prevRoll)
          sndTurn = nextPosition sndPos (last prevRoll + 3)

fstPart :: IO ()
fstPart = parseInput >>= print . simulate [0] 0 . map (,0)