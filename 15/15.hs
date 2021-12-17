import qualified Data.Heap as Heap
import qualified Data.Matrix as Matrix
import Data.Maybe (isJust, fromJust)
import Data.Functor ((<&>))
import Data.List (sortBy)
import Data.Function (on)

type Distance = Integer
type Location = (Int, Int)
type RiskLevel = Integer
type RiskLevelMap = Matrix.Matrix (RiskLevel, Distance)
type Queue = [Location]
type MinimumHeap = Heap.MinPrioHeap Distance Location

upperBound :: Integer
upperBound = 10^7

unsafeRead :: String -> (RiskLevel, Distance)
unsafeRead s = (read s :: Integer, upperBound)

parseInput :: IO RiskLevelMap
parseInput = fmap (map (map (unsafeRead . (:[]))) . lines) (readFile "15.in") <&> Matrix.fromLists

isValidLocation :: RiskLevelMap -> Location -> Bool
isValidLocation riskLevelMap (r, c) = isJust $ Matrix.safeGet r c riskLevelMap

getNeighbours :: RiskLevelMap -> Location -> [Location]
getNeighbours riskLevelMap (r, c) = filter (isValidLocation riskLevelMap) [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]

decreasePriority :: MinimumHeap -> Location -> Distance -> MinimumHeap
decreasePriority heap location newDistance = Heap.insert (newDistance, location) without
    where without = Heap.fromList $ filter (\(_, loc) -> loc /= location) $ Heap.toList heap

relaxNeighbours :: MinimumHeap -> RiskLevelMap -> Location -> [Location] -> (MinimumHeap, RiskLevelMap)
relaxNeighbours heap riskLevelMap _ [] = (heap, riskLevelMap)
relaxNeighbours heap riskLevelMap current (n:ns)
    | currentDistance + neighboursRiskLevel < neighboursDistance = relaxNeighbours relaxedHeap relaxedRiskLevelMap current ns
    | otherwise = relaxNeighbours heap riskLevelMap current ns
    where (currentRiskLevel, currentDistance) = riskLevelMap Matrix.! current
          (neighboursRiskLevel, neighboursDistance) = riskLevelMap Matrix.! n
          relaxed = (neighboursRiskLevel, currentDistance + neighboursRiskLevel)
          relaxedRiskLevelMap = Matrix.setElem relaxed n riskLevelMap
          relaxedHeap = decreasePriority heap n (currentDistance + neighboursRiskLevel)

dijkstra :: RiskLevelMap -> MinimumHeap -> RiskLevelMap
dijkstra riskLevelMap heap
    | Heap.isEmpty heap = riskLevelMap
    | otherwise = dijkstra relaxedRiskLevelMap relaxedHeap
    where (currentDistance, currentLocation) = fromJust $ Heap.viewHead heap
          neighbours = getNeighbours riskLevelMap currentLocation
          (relaxedHeap, relaxedRiskLevelMap) = relaxNeighbours (Heap.drop 1 heap) riskLevelMap currentLocation neighbours

fstPart :: IO Integer
fstPart = do
    riskLevelMap <- parseInput
    let entryRiskLevel = riskLevelMap Matrix.! (1, 1)
    let initRiskLevelMap = Matrix.setElem (fst entryRiskLevel, 0) (1, 1) riskLevelMap
    let initMinHeap = Heap.fromList ((0, (1, 1)) : tail [(upperBound, (r, c)) | r <- [1..Matrix.nrows initRiskLevelMap], c <- [1..Matrix.ncols initRiskLevelMap]])
    return $ snd $ dijkstra initRiskLevelMap initMinHeap Matrix.! (Matrix.nrows initRiskLevelMap, Matrix.ncols initRiskLevelMap)

addOne :: RiskLevelMap -> RiskLevelMap
addOne = fmap (\(e, d) -> if e == 9 then (1, d) else if e == 8 then (9, d) else (mod (e + 1) 9, d))

rowAdd :: RiskLevelMap -> Int -> [RiskLevelMap]
rowAdd _ 0 = []
rowAdd riskLevelMap n = added : rowAdd added (n - 1)
    where added = addOne riskLevelMap