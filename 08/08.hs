import Data.List (intersect, foldl')
type DigitSignal = String
type Output = String

digitsCount :: Int
digitsCount = 10

parseInput :: IO [([DigitSignal], [Output])]
parseInput = do
    inLines <- fmap (map words . lines) (readFile "08.in")
    return $ map (\l -> (take digitsCount l, drop (digitsCount + 1) l)) inLines

isFstPartDigit :: (Eq a, Num a) => a -> Bool
isFstPartDigit x = x == 2 || x == 3 || x == 4 || x == 7

fstPartDigitCount :: [String] -> Int
fstPartDigitCount = length . filter isFstPartDigit . map length

fstPart :: IO ()
fstPart = parseInput >>= print . foldl' (\acc (_, o) -> acc + fstPartDigitCount o) 0