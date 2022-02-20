import Numeric (showIntAtBase, readHex)
import Data.List (foldl')
import Data.Char (digitToInt, intToDigit)
import qualified Data.Map as Map

type BinaryNum = String
type Bit = Char
type HexaNum = String
type TypeId = Int
type Version = Int

data Packet = Outermost HexaNum
            | LiteralValue Version TypeId Int
            | Operator Version TypeId [Packet]
            deriving Show

fullBinHexas :: [String]
fullBinHexas = ["0000", "0001", "0010", "0011",
               "0100", "0101", "0110", "0111",
               "1000", "1001", "1010", "1011",
               "1100", "1101", "1110", "1111"]

hexaNums :: Map.Map Char String
hexaNums = Map.fromList $ zip (['0'..'9'] ++ ['A'..'F']) fullBinHexas

parseInput :: IO Packet
parseInput = fmap (Outermost . head . lines) (readFile "16.in")

intToBin :: Int -> BinaryNum
intToBin n = showIntAtBase 2 intToDigit n ""

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

pruneEndingZeros :: BinaryNum -> BinaryNum
pruneEndingZeros = reverse' . dropWhile (== '0') . reverse'

hexaToBin :: HexaNum -> BinaryNum
hexaToBin = pruneEndingZeros . concatMap (hexaNums Map.!)

decFromBin :: BinaryNum -> Int
decFromBin = foldl' (\acc x -> acc * 2 + digitToInt x) 0

extractLiteralValue :: BinaryNum -> (Int, Int)
extractLiteralValue n = (decFromBin $ concat parsedVal, length parsedVal)
    where parsedVal = parseLiteralValue n
          parseLiteralValue :: BinaryNum -> [[Bit]]
          parseLiteralValue n
              | head n == '1' = selectFour : parseLiteralValue (drop 5 n)
              | otherwise = [selectFour]
              where selectFour = take 4 $ tail n

parsePacket :: BinaryNum -> (Packet, BinaryNum)
parsePacket n = case typeId of
    4 -> (LiteralValue version typeId literalValue, drop (5 * bitGroupsCount) afterHeader)
    _ -> case lengthTypeId of
        0 -> (Operator version typeId (decodePackets $ take totalLengthInBits $ drop 16 afterHeader), drop (totalLengthInBits + 16) afterHeader)
        _ -> (Operator version typeId (map fst nTimesSubPackets), drop (sum (map snd nTimesSubPackets) + 12) afterHeader)
    where version = decFromBin $ take 3 n
          typeId = decFromBin $ take 3 $ drop 3 n
          afterHeader = drop 6 n
          lengthTypeId = digitToInt $ head afterHeader
          totalLengthInBits = decFromBin $ take 15 $ drop 1 afterHeader
          numberOfSubpackets = decFromBin $ take 11 $ drop 1 afterHeader
          (literalValue, bitGroupsCount) = extractLiteralValue afterHeader
          nTimesSubPackets = init $ decodeNPackets (drop 12 afterHeader) numberOfSubpackets

decodePackets :: BinaryNum -> [Packet]
decodePackets n
    | null rest = [newPacket]
    | otherwise = newPacket : decodePackets rest
    where (newPacket, rest) = parsePacket n

decodeNPackets :: BinaryNum -> Int -> [(Packet, Int)]
decodeNPackets _ 0 = [(Outermost "", 0)]
decodeNPackets decodeFrom n = (newPacket, length decodeFrom - length rest) : decodeNPackets rest (n - 1)
    where (newPacket, rest) = parsePacket decodeFrom

versionSum :: [Packet] -> Int
versionSum [] = 0
versionSum (p:ps) = case p of
    (LiteralValue ver _ _) -> ver + versionSum ps
    (Operator ver _ packets) -> ver + versionSum packets + versionSum ps

solve :: Show a => ([Packet] -> a) -> IO ()
solve f = parseInput >>= (\(Outermost hexaNum) -> print $ f $ decodePackets $ hexaToBin hexaNum)

fstPart :: IO ()
fstPart = solve versionSum

evaluatePackets :: Packet -> Int
evaluatePackets p = case p of
    (LiteralValue _ _ val) -> val
    (Operator _ typeId subPackets) -> case typeId of
        0 -> sum $ map evaluatePackets subPackets
        1 -> product $ map evaluatePackets subPackets
        2 -> minimum $ map evaluatePackets subPackets
        3 -> maximum $ map evaluatePackets subPackets
        5 -> if evaluatePackets (head subPackets) > evaluatePackets (last subPackets) then 1 else 0
        6 -> if evaluatePackets (head subPackets) < evaluatePackets (last subPackets) then 1 else 0
        7 -> if evaluatePackets (head subPackets) == evaluatePackets (last subPackets) then 1 else 0

sndPart :: IO ()
sndPart = solve (evaluatePackets . head)