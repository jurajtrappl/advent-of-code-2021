import Data.Bifunctor (Bifunctor(bimap))
import Data.List.Split (splitWhen, splitOn)
import qualified Data.Matrix as Matrix

type Position = (Int, Int)
type Fold = (Char, Int)
type ThermalMap = Matrix.Matrix String
type Size = (Int, Int)

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

parsePosition :: String -> (Int, Int)
parsePosition s = let splitted = splitOn "," s
                  in (unsafeReadInt $ head splitted, unsafeReadInt $ last splitted)

parseFold :: String -> (Char, Int)
parseFold s = let splitted = splitOn "=" s
              in (last $ head splitted, unsafeReadInt $ last splitted)

parseInput :: IO ([Position], [Fold])
parseInput = do
    sections <- fmap (splitWhen (== "") . lines) (readFile "13.in")
    return $ bimap (map parsePosition) (map parseFold) (head sections, last sections)

findDimensions :: [Position] -> Size
findDimensions positions = bimap maximum maximum (map snd positions, map fst positions)

emptyMap :: Size -> ThermalMap
emptyMap (nrows, ncols) = Matrix.fromLists $ replicate (nrows + 1) oneRow
    where oneRow = replicate (ncols + 1) ((:[]) '.')

pickSymbol :: (String, String) -> String
pickSymbol (a, b) = if a == "#" || b == "#" then "#" else "."

addMaps :: ThermalMap -> ThermalMap -> ThermalMap
addMaps first second = Matrix.fromLists $ map (map pickSymbol) zipped
    where zipped = zipWith zip (Matrix.toLists first) (Matrix.toLists second)

foldByRow :: ThermalMap -> Int -> ThermalMap
foldByRow thermalMap r = addMaps upperMap (Matrix.fromLists $ reverse $ Matrix.toLists bottomMap)
    where upperMap = Matrix.submatrix 1 r 1 (Matrix.ncols thermalMap) thermalMap
          bottomMap = Matrix.submatrix (r + 2) (Matrix.nrows thermalMap) 1 (Matrix.ncols thermalMap) thermalMap

foldByCol :: ThermalMap -> Int -> ThermalMap
foldByCol thermalMap c = addMaps leftMap (Matrix.fromLists $ map reverse $ Matrix.toLists rightMap)
    where leftMap = Matrix.submatrix 1 (Matrix.nrows thermalMap) 1 c thermalMap
          rightMap = Matrix.submatrix 1 (Matrix.nrows thermalMap) (c + 2) (Matrix.ncols thermalMap) thermalMap

fold :: ThermalMap -> Int -> Char -> ThermalMap
fold thermalMap num instr = if instr == 'y'
                                then foldByRow thermalMap num
                                else foldByCol thermalMap num

buildThermalMap :: [Position] -> ThermalMap
buildThermalMap positions = foldl (\acc (c, r) -> Matrix.setElem "#" (r + 1, c + 1) acc) (emptyMap $ findDimensions positions) positions

fstPart :: IO ()
fstPart = do
    (positions, folds) <- parseInput
    let thermalMap = buildThermalMap positions
    let (instr, num) = head folds
    print $ length $ filter (== "#") $ Matrix.toList $ fold thermalMap num instr

sndPart :: IO ()
sndPart = do
    (positions, folds) <- parseInput
    let thermalMap = buildThermalMap positions
    print $ foldl (\acc (instr, num) -> fold acc num instr) thermalMap folds