import Data.List (foldl')
import Data.List.Split (splitOn)

type Position = (Int, Int)
type Range = [Int]

data Probe = Probe Position Position deriving Show
data TargetArea = TargetArea Range Range deriving Show

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

parseRange :: String -> Range
parseRange = map unsafeReadInt . splitOn ".."

parseInput :: IO TargetArea
parseInput = do
    ranges <- fmap (concatMap (drop 1 . splitOn "=") . drop 2 . words) (readFile "17.in")
    let xRange = parseRange $ head $ map (filter (/= ',')) ranges
    let yRange = parseRange $ last ranges
    return $ TargetArea [head xRange..last xRange] [head yRange..last yRange]

makeStep :: Probe -> Probe
makeStep (Probe (currentX, currentY) (velocityX, velocityY))
    | velocityX > 0 = Probe newPosition (velocityX - 1, gravityDecrease)
    | velocityX < 0 = Probe newPosition (velocityX + 1, gravityDecrease)
    | otherwise = Probe newPosition (velocityX, gravityDecrease)
    where newPosition = (currentX + velocityX, currentY + velocityY)
          gravityDecrease = velocityY - 1

isInTargetArea :: TargetArea -> Position -> Bool
isInTargetArea (TargetArea xRange yRange) (x, y) = x `elem` xRange && y `elem` yRange

isOverTargetArea :: TargetArea -> Position -> Bool
isOverTargetArea (TargetArea xRange yRange) (x, y) = x > last xRange || y < head yRange

canLandInTargetArea :: TargetArea -> Probe -> Bool
canLandInTargetArea targetArea p@(Probe currentPosition velocity)
    | isInTargetArea targetArea currentPosition = True
    | isOverTargetArea targetArea currentPosition = False
    | otherwise = canLandInTargetArea targetArea (makeStep p)

generateVelocities :: TargetArea -> [Position]
generateVelocities (TargetArea xRange yRange) = [(x, y) | x <- [1..500], y <- [-500..500]]

tryVelocities :: TargetArea -> [(Position, Bool)]
tryVelocities targetArea = map (\v -> (v, canLandInTargetArea targetArea (Probe (0, 0) v))) velocityCandidates
    where velocityCandidates = generateVelocities targetArea

findYCoords :: TargetArea -> Probe -> [Int]
findYCoords targetArea p@(Probe currentPosition velocity)
    | isInTargetArea targetArea currentPosition = []
    | otherwise = snd newPosition : findYCoords targetArea newP
    where newP@(Probe newPosition _) = makeStep p

getHighestY :: [[Int]] -> Int
getHighestY = maximum . map maximum

fstPart :: IO ()
fstPart = do
    targetArea <- parseInput
    print $ getHighestY $ map ((findYCoords targetArea . Probe (0, 0)). fst) $ filter snd $ tryVelocities targetArea

sndPart :: IO ()
sndPart = parseInput >>= print . length . filter snd . tryVelocities