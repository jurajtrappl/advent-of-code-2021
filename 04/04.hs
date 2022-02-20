import Data.List.Split (splitOn, chunksOf)

type Draw = Int
data Tile = Tile Int Bool deriving (Eq, Show)
type Row = [Tile]
newtype Board = Board [Row]

gridSize :: Int
gridSize = 5

tileInit :: Int -> Tile
tileInit number = Tile number False

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

parseInput :: IO ([Draw], [Board])
parseInput = do
    fContent <- readFile "04.in"
    let lined = filter (/= "") $ lines fContent
    let draws = map unsafeReadInt $ splitOn "," $ head lined
    let unprocBoards = map (map (map (tileInit . unsafeReadInt) . words)) $ chunksOf gridSize $ tail lined
    return (draws, map Board unprocBoards)

isFullMarkedRow :: Row -> Bool
isFullMarkedRow = all (\(Tile _ isMarked) -> isMarked)

transpose :: Eq a => [[a]] -> [[a]]
transpose m
    | not (any (/= []) m) = []
    | otherwise = map head m : transpose (map (drop 1) m)

isWinningBoard :: Board -> Bool
isWinningBoard (Board rows) = any isFullMarkedRow rows || any isFullMarkedRow (transpose rows)

markTile :: Draw -> Tile -> Tile
markTile d t@(Tile num isMarked) = if num == d then Tile num True else t

markBoard :: Draw -> Board -> Board
markBoard d (Board rows) = Board (map (map (markTile d)) rows)

unmarkedSum :: Board -> Int
unmarkedSum (Board rows) = foldl (\acc (Tile num _) -> acc + num) 0 . filter (\(Tile _ isMarked) -> not isMarked) $ concat rows

playBingo :: ([Draw], [Board]) -> (Draw, Board)
playBingo ([], _) = (-1, Board [])
playBingo (d:ds, boards)
    | any isWinningBoard markedBoards = (d, head $ filter isWinningBoard markedBoards)
    | otherwise = playBingo (ds, markedBoards)
    where markedBoards = map (markBoard d) boards

fstPart :: IO ()
fstPart = parseInput >>= print . (\(draw, b) -> draw * unmarkedSum b) . playBingo

squidBingo :: ([Draw], [Board]) -> [(Draw, [Board])]
squidBingo ([], _) = [(-1, [])]
squidBingo (d:ds, boards) = (d, filter isWinningBoard markedBoards) : squidBingo (ds, filter (not . isWinningBoard) markedBoards)
    where markedBoards = map (markBoard d) boards

sndPart :: IO ()
sndPart = parseInput >>= print . (\(draw, b) -> draw * unmarkedSum (head b)) . last . filter (\(_, bs) -> not $ null bs) . squidBingo