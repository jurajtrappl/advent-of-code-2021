import qualified Data.Matrix as Matrix
import Data.Maybe (isJust, fromJust)
import Data.Functor ((<&>))
import Data.List (foldl')

type Location = (Int, Int)
type RiskLevel = Integer
type RiskLevelMap = Matrix.Matrix Int
type SumMap = Matrix.Matrix Int

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

parseInput :: IO RiskLevelMap
parseInput = fmap (map (map (unsafeReadInt . (:[]))) . lines) (readFile "15.in") <&> Matrix.fromLists

indices :: RiskLevelMap -> [Location]
indices riskLevelMap = [(x, y) | x <- [1..Matrix.nrows riskLevelMap], y <- [1..Matrix.ncols riskLevelMap]]

isValidLocation :: Location -> SumMap -> Bool
isValidLocation (x, y) = isJust . Matrix.safeGet x y

getGridNeighbours :: SumMap -> Location -> [Location]
getGridNeighbours sumMap l@(x, y) = filter (`isValidLocation` sumMap) [(x, y - 1), (x - 1, y)]

modifyLocation :: RiskLevelMap -> SumMap -> Location -> SumMap
modifyLocation riskLevelMap sumMap l@(x, y) = Matrix.setElem newValue l sumMap
    where neighboursValues = map (sumMap Matrix.!) $ getGridNeighbours sumMap l
          newValue = if null neighboursValues then 0 else minimum neighboursValues + riskLevel
          riskLevel = riskLevelMap Matrix.! l

fstPart :: IO ()
fstPart = do
    riskLevelMap <- parseInput
    let sumMap = Matrix.zero (Matrix.ncols riskLevelMap) (Matrix.nrows riskLevelMap)
    let summed = foldl (modifyLocation riskLevelMap) sumMap (indices riskLevelMap)
    print $ summed Matrix.! (Matrix.nrows sumMap, Matrix.ncols sumMap)

addOne :: RiskLevelMap -> RiskLevelMap
addOne = fmap (\e -> if e == 9 then 1 else if e == 8 then 9 else mod (e + 1) 9)

rowAdd :: RiskLevelMap -> Int -> [RiskLevelMap]
rowAdd _ 0 = []
rowAdd riskLevelMap n = added : rowAdd added (n - 1)
    where added = addOne riskLevelMap

constructGrid :: RiskLevelMap -> RiskLevelMap
constructGrid first = foldl' (Matrix.<->) firstRow [secondRow, thirdRow, fourthRow, fifthRow]
    where firstRow = foldl' (Matrix.<|>) first (rowAdd first 4)
          secondRow = foldl' (Matrix.<|>) (addOne first) (rowAdd (addOne first) 4)
          thirdRow = foldl' (Matrix.<|>) (addOne $ addOne first) (rowAdd (addOne $ addOne first) 4)
          fourthRow = foldl' (Matrix.<|>) (addOne $ addOne $ addOne first) (rowAdd (addOne $ addOne $ addOne first) 4)
          fifthRow = foldl' (Matrix.<|>) (addOne $ addOne $ addOne $ addOne first) (rowAdd (addOne $ addOne $ addOne $ addOne first) 4)

sndPart :: IO ()
sndPart = do
    riskLevelMap <- parseInput
    let extendedRiskLevelMap = constructGrid riskLevelMap
    let sumMap = Matrix.zero (Matrix.ncols extendedRiskLevelMap) (Matrix.nrows extendedRiskLevelMap)
    let summed = foldl' (modifyLocation extendedRiskLevelMap) sumMap (indices extendedRiskLevelMap)
    print $ summed Matrix.! (Matrix.nrows sumMap, Matrix.ncols sumMap)