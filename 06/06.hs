import Data.List.Split (splitOn)

type Days = Int
type SpawnRate = Int

parseInput :: IO [SpawnRate]
parseInput = fmap (map (\s -> read s :: SpawnRate) . splitOn ",") (readFile "06.in")

decrease :: [SpawnRate] -> [SpawnRate]
decrease = map (\rate -> rate - 1)

modifyAfterSpawning :: [SpawnRate] -> Int -> [SpawnRate]
modifyAfterSpawning rates zerosCount = map (\r -> if r == 0 then 6 else r - 1) rates ++ replicate zerosCount 8

simulate :: Days -> [SpawnRate] -> [SpawnRate]
simulate 0 spawnrates = spawnrates
simulate days spawnrates
    | not $ null zeros = simulate (days - 1) (modifyAfterSpawning spawnrates (length zeros))
    | otherwise = simulate (days - 1) (decrease spawnrates)
    where zeros = filter (==0) spawnrates

fstPart :: IO ()
fstPart = parseInput >>= print . length . simulate 80

sndPart :: IO ()
sndPart = parseInput >>= print . length . simulate 256
