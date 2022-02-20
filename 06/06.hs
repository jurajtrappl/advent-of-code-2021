import Data.List.Split (splitOn)
import qualified Data.Map as Map

type Days = Int
type SpawnRate = Int

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

parseInput :: IO [SpawnRate]
parseInput = fmap (map unsafeReadInt . splitOn ",") (readFile "06.in")

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

countOccurences :: Int -> [SpawnRate] -> [Int]
countOccurences (-1) _ = []
countOccurences num rates = length (filter (==num) rates) : countOccurences (num - 1) rates

sndPart :: IO ()
sndPart = do
    spawnrates <- parseInput
    let mem = Map.fromList $ zip [0..8] (reverse $ countOccurences 8 spawnrates)
    print $ sum $ map snd $ Map.toList $ foldl (\acc d -> Map.updateAt (\_ _ -> Just ((acc Map.! mod d 9) + acc Map.! mod (d + 7) 9)) (mod (d + 7) 9) acc) mem [0..255]
