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
    where algoIndex = toIndex $ neighbourPixels inputImg (x - 2, y - 2)

darkPixel :: Char
darkPixel = '.'

enhance :: Image -> ImageEnhanceAlg -> Int -> Image
enhance inputImg _ 0 = inputImg
enhance inputImg algo n = enhance enhancedOutputImg algo (n - 1)
    where (nrows, ncols) = (Matrix.nrows inputImg + 4, Matrix.ncols inputImg + 4)
          outputImg = Matrix.fromList nrows ncols $ replicate (nrows * ncols) darkPixel
          outputLocations = [(x, y) | x <- [1..nrows], y <- [1..ncols]]
          enhancedOutputImg = foldl' (\acc l -> scanInput inputImg acc algo l) outputImg outputLocations
                                                                                                                                                                                                                                                                                                                                                                                                                                                               
fstPart :: IO ()
fstPart = do
    (algo, inputImg) <- parseInput
    let enhancedTwoTimes = enhance inputImg algo 2
    print $ length $ filter (== '#') $ Matrix.toList enhancedTwoTimes