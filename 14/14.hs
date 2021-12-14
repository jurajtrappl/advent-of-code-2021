import qualified Data.Map as Map
import Data.List.Split (splitWhen, splitOn)
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (foldl', sortBy)
import Data.Function (on)

type PolymerTemplate = String
type PairInsertionRule = (String, Char)
type InsertionRules = Map.Map String Char

parsePairInsertionRule :: String -> PairInsertionRule
parsePairInsertionRule r = let splitted = splitOn " -> " r
                           in (head splitted, head $ last splitted)

parseInput :: IO (PolymerTemplate, InsertionRules)
parseInput = do
    sections <- fmap (splitWhen (== "") . lines) (readFile "14.in")
    return $ bimap head (Map.fromList . map parsePairInsertionRule) (head sections, last sections)

windowsOf :: Int -> [a] -> [[a]]
windowsOf size (x:xs)
    | length xs >= size = (x : take (size - 1) xs) : windowsOf size xs
    | otherwise = [x:xs]

applyInsertionRule :: InsertionRules -> String -> String
applyInsertionRule rules pair = head pair : rules Map.! pair : tail pair

mergeAfterInsertion :: [String] -> String
mergeAfterInsertion = foldl1 (\acc p -> init acc ++ p)

loop :: Int -> InsertionRules -> [String] -> String
loop 0 _ x = mergeAfterInsertion x
loop n rules x = loop (n - 1) rules (windowsOf 2 $ mergeAfterInsertion (map (applyInsertionRule rules) x))

countChar :: Char -> Map.Map Char Int -> Map.Map Char Int
countChar c = Map.insertWith (+) c 1

countOccurences :: String -> Map.Map Char Int
countOccurences = foldl' (flip countChar) Map.empty

solve :: Int -> IO ()
solve nSteps = do
    (polymerTemplate, rules) <- parseInput
    let looped = loop nSteps rules (windowsOf 2 polymerTemplate)
    let sortedOccurences = sortBy (compare `on` snd) $ Map.toList $ countOccurences looped
    print $ snd (last sortedOccurences) - snd (head sortedOccurences)

fstPart :: IO ()
fstPart = solve 10

sndPart :: IO ()
sndPart = solve 40