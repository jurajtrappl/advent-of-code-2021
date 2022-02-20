import qualified Data.Matrix as Matrix
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Data.Char (digitToInt)
import Data.List (foldl')

type Location = (Int, Int)
type ImageEnhanceAlg = String
type Image = Matrix.Matrix Char

parseInput :: IO (ImageEnhanceAlg, Image)
parseInput = do
    fContent <- fmap (filter (/= "") . lines) (readFile "20.in")
    return (head fContent, Matrix.fromLists $ tail fContent)

lightPixel :: Char
lightPixel = '#'

frame :: ImageEnhanceAlg -> Int -> Char
frame algo n
    | head algo == darkPixel = darkPixel
    | head algo == lightPixel && algo !! 511 == darkPixel = if odd n then darkPixel else lightPixel
    | otherwise = if n == 1 then darkPixel else lightPixel

toBinChar :: Maybe Char -> Char
toBinChar mc = case mc of
    Just '#' -> '1'
    _ -> '0'

toDecimal :: String -> Int
toDecimal = foldl' (\acc x -> acc * 2 + digitToInt x) 0

neighbourPixels :: Image -> Location -> [Maybe Char]
neighbourPixels inputImg (x, y) = map (\(x', y') -> Matrix.safeGet x' y' inputImg) pixelsAround
    where pixelsAround = [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1]]

toIndex :: [Maybe Char] -> Int
toIndex = toDecimal . map toBinChar

scanInput :: Image -> Image -> ImageEnhanceAlg -> Location -> Image
scanInput inputImg outputImg algo location@(x, y) = Matrix.setElem (algo !! algoIndex) location outputImg
    where algoIndex = toIndex $ neighbourPixels inputImg (x - 1, y - 1)

darkPixel :: Char
darkPixel = '.'

extendFrame :: Image -> ImageEnhanceAlg -> Int -> Image
extendFrame enhanceOutput algo iteration = Matrix.fromLists $ [firstOrLastRow] ++ body ++ [firstOrLastRow]
    where frameChar = frame algo iteration
          firstOrLastRow = replicate (Matrix.ncols enhanceOutput + 2) frameChar
          body = map (\row -> [frameChar] ++ row ++ [frameChar]) $ Matrix.toLists enhanceOutput
          
enhance :: Image -> ImageEnhanceAlg -> Int -> Int -> Image
enhance inputImg algo current n
    | current == n = inputImg
    | otherwise = enhance (extendFrame enhancedOutputImg algo current) algo (current + 1) n
    where (nrows, ncols) = (Matrix.nrows inputImg + 2, Matrix.ncols inputImg + 2)
          outputImg = Matrix.fromList nrows ncols $ replicate (nrows * ncols) darkPixel
          outputLocations = [(x, y) | x <- [1..nrows], y <- [1..ncols]]
          enhancedOutputImg = foldl' (\acc l -> scanInput inputImg acc algo l) outputImg outputLocations
                                                                                                                                                                                                                                                                                                                                                                                                                                                               
fstPart :: IO ()
fstPart = do
    (algo, inputImg) <- parseInput
    let enhancedTwoTimes = enhance inputImg algo 1 2
    print $ length $ filter (== '#') $ Matrix.toList enhancedTwoTimes