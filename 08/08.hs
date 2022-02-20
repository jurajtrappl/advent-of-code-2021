import qualified Data.List as List
import qualified Data.Map as Map
import Data.List (sort)
import Data.Functor((<&>))

type DigitSignal = String
type Output = String

parseInput :: IO [([DigitSignal], [Output])]
parseInput = readFile "08.in" <&> map ((\l -> (take 10 l, drop 11 l)) . words) . lines

isFstPartDigit :: (Eq a, Num a) => a -> Bool
isFstPartDigit x = x == 2 || x == 3 || x == 4 || x == 7

fstPartDigitCount :: [String] -> Int
fstPartDigitCount = length . filter isFstPartDigit . map length

fstPart :: IO ()
fstPart = parseInput >>= print . List.foldl' (\acc (_, o) -> acc + fstPartDigitCount o) 0

sticks :: Map.Map Integer Integer
sticks = Map.fromList $ zip [0..6] [8, 6, 8, 7, 4, 9, 7]

countChar :: Char -> Map.Map Char Int -> Map.Map Char Int
countChar c = Map.insertWith (+) c 1

countCipherOccurences :: String -> Map.Map Char Int
countCipherOccurences = List.foldl' (flip countChar) Map.empty

getKeyByValue :: Eq b => b -> [(c, b)] -> c
getKeyByValue value = fst . head . filter (\(_, v) -> v == value)

uniqueSticks :: Map.Map Char Int -> [(Char, Char)] -- b6, e4, f9
uniqueSticks cipher = ('b', b) : ('e', e) : [('f', f)]
    where cipherList = Map.toList cipher
          b = getKeyByValue 6 cipherList
          e = getKeyByValue 4 cipherList
          f = getKeyByValue 9 cipherList

sticksAc :: Map.Map Char Int -> [DigitSignal] -> [(Char, Char)] -- uppermost and right down
sticksAc cipher digits = ('a', a) : [('c', fst $ head $ filter (\(k, v) -> k /= a && v == 8) $ Map.toList cipher)]
    where digitsLengths = map (\d -> (d, length d)) digits
          seven = getKeyByValue 3 digitsLengths
          one = getKeyByValue 2 digitsLengths
          a = head $ seven List.\\ one

sticksDg :: Map.Map Char Char -> [[Char]] -> [(Char, Char)] -- second and third horizontal
sticksDg cipher digits = ('d', head $ filter (`notElem` alreadyMapped) differences) : [('g', head $ "abcdefg" List.\\ (alreadyMapped ++ "d"))]
    where digitsLengths = map (\d -> (d, length d)) digits
          eight = getKeyByValue 7 digitsLengths
          zeroSixNine = map fst $ filter (\(_, l) -> l == 6) digitsLengths
          differences = concatMap (eight List.\\) zeroSixNine
          alreadyMapped = map snd (Map.toList cipher)

decipher :: [DigitSignal] -> Map.Map Char Char
decipher digits = Map.fromList (sticksDg (Map.fromList withUniqueAndAc) digits ++ withUniqueAndAc)
    where occurences = countCipherOccurences $ concat digits
          withUniqueAndAc = uniqueSticks occurences ++ sticksAc occurences digits

decodeDigits :: Map.Map Char Char -> [DigitSignal] -> Map.Map String Int
decodeDigits deciphered digits = Map.fromList $ zip (map sort [zero, one, two, three, four, five, six, seven, eight, nine]) [0..9]
    where digitsLengths = map (\d -> (d, length d)) digits
          zero = fst $ head $ filter (\(d, _) -> (deciphered Map.! 'd') `notElem` d) $ filter (\(_, l) -> l == 6) digitsLengths
          one = getKeyByValue 2 digitsLengths
          two = fst $ head $ filter (\(d, _) -> (deciphered Map.! 'e') `elem` d) $ filter (\(_, l) -> l == 5) digitsLengths
          three = fst $ head $ filter (\(d, _) -> (deciphered Map.! 'c') `elem` d && (deciphered Map.! 'f') `elem` d) $ filter (\(_, l) -> l == 5) digitsLengths
          four = getKeyByValue 4 digitsLengths
          five = fst $ head $ filter (\(d, _) -> (deciphered Map.! 'b') `elem` d) $ filter (\(_, l) -> l == 5) digitsLengths
          six = fst $ head $ filter (\(d, _) -> (deciphered Map.! 'f') `elem` d && (deciphered Map.! 'c') `notElem` d) $ filter (\(_, l) -> l == 6) digitsLengths
          seven = getKeyByValue 3 digitsLengths
          eight = getKeyByValue 7 digitsLengths
          nine = fst $ head $ filter (\(d, _) -> (deciphered Map.! 'b') `elem` d && (deciphered Map.! 'e') `notElem` d) $ filter (\(_, l) -> l == 6) digitsLengths

translateOutput :: ([DigitSignal], [Output]) -> Int
translateOutput (digits, output) = foldl (\acc num -> acc * 10 + decodedDigits Map.! sort num) 0 output
    where deciphered = decipher digits
          decodedDigits = decodeDigits deciphered digits

sndPart :: IO Int
sndPart = sum . map translateOutput <$> parseInput
