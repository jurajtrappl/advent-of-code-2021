import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Bifunctor (Bifunctor(second))

parseInput :: IO [String]
parseInput = fmap lines (readFile "03.in")

binDigitsCounts :: String -> (Int, Int)
binDigitsCounts num = (length $ filter (=='0') num, length $ filter (=='1') num)

getCols :: Eq a => [[a]] -> [[a]]
getCols m
    | not (any (/= []) m) = []
    | otherwise = map head m : getCols (map (drop 1) m)

mostCommon :: Ord a => (a, a) -> Bool
mostCommon (a, b) = a > b

leastCommon :: Ord a => (a, a) -> Bool
leastCommon (a, b) = a <= b

fstPartRate :: ((Int, Int) -> Bool) -> [(Int, Int)] -> String
fstPartRate comparison = foldl (\acc new -> if comparison new then acc ++ "0" else acc ++ "1") ""

decFromBinStr :: String -> Int
decFromBinStr = foldl' (\acc x -> acc * 2 + digitToInt x) 0

fstPart :: IO ()
fstPart = do
    matrix <- parseInput
    let counts = map binDigitsCounts $ getCols matrix
    print $ decFromBinStr (fstPartRate mostCommon counts) * decFromBinStr (fstPartRate leastCommon counts)

filterNumsStartingWith :: Char -> [(Int, String)] -> [(Int, String)]
filterNumsStartingWith char = map (second (drop 1)) . filter (\(_, n) -> head n == char)

sndPartRate :: ((Int, Int) -> Bool) -> [(Int, String)] -> Int
sndPartRate comparison [x] = fst x
sndPartRate comparison indicesWithNums
    | comparison counts = sndPartRate comparison (filterNumsStartingWith '1' indicesWithNums)
    | otherwise = sndPartRate comparison (filterNumsStartingWith  '0' indicesWithNums)
    where counts = binDigitsCounts $ map (head . snd) indicesWithNums

sndPart :: IO ()
sndPart = do
    matrix <- parseInput
    let indicesWithNums = zip [0..length matrix - 1] matrix
    print $ decFromBinStr (matrix !! sndPartRate mostCommon indicesWithNums) * decFromBinStr (matrix !! sndPartRate leastCommon indicesWithNums)