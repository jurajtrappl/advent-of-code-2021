{-# OPTIONS_GHC -Wno-missing-methods #-}

data Point = P Int Int

instance Num Point where
    (P x y) + (P x' y') = P (x + x') (y + y')

parseCommand :: String -> Point
parseCommand value = case head splitted of
    "forward" -> P num 0
    "down" -> P 0 (-num)
    _ -> P 0 num
    where splitted = words value
          num = read (last splitted) :: Int

parseInput :: IO [Point]
parseInput = fmap (map parseCommand . lines) (readFile "02.in")

computeResult :: Point -> Int
computeResult (P x y) = abs (x * y)

fstPart :: IO ()
fstPart = parseInput >>= (print . computeResult . foldr (+) (P 0 0))
