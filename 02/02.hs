data Direction = Forward Int
               | Down Int
               | Up Int
    deriving (Show)

type Aim = Int
type Depth = Int
type Horizontal = Int

data SimpleState = S Horizontal Depth
data ComplexState = C Horizontal Depth Aim

parseCommand :: String -> Direction
parseCommand value = case head splitted of
    "forward" -> Forward num
    "down" -> Down num
    _ -> Up num
    where splitted = words value
          num = read (last splitted) :: Int

parseInput :: IO [Direction]
parseInput = fmap (map parseCommand . lines) (readFile "02.in")

executeSimple :: SimpleState -> Direction -> SimpleState
executeSimple (S h d) cmd = case cmd of
    Down num -> S h (d + num)
    Up num -> S h (d - num)
    Forward num -> S (h + num) d

compute :: Show b1 => (b2 -> b1) -> (b2 -> Direction -> b2) -> b2 -> IO ()
compute formatResult executor initial =
    parseInput >>= (print . formatResult . foldl executor initial)

fstPart :: IO ()
fstPart = compute (\(S x y) -> x * y) executeSimple (S 0 0)

executeComplex :: ComplexState -> Direction -> ComplexState
executeComplex (C h d a) cmd = case cmd of
    Down num -> C h d (a + num)
    Up num -> C h d (a - num)
    Forward num -> C (h + num) (d + a * num) a

sndPart :: IO ()
sndPart = compute (\(C h d _) -> h * d) executeComplex (C 0 0 0)