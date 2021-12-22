import Data.Char (digitToInt, isDigit)
import Data.List (foldl')

data Snailfish = Simple Int
               | Complex Snailfish Snailfish
               deriving Show

parseInput :: IO [Snailfish]
parseInput = fmap (map parseSnailfish . lines) (readFile "18.in")

parseSnailfish :: String -> Snailfish
parseSnailfish s = Complex (head parsed) (last parsed)
    where parsed = parseSnailfishInner s []
          parseSnailfishInner :: String -> [Snailfish] -> [Snailfish]
          parseSnailfishInner [] _ = []
          parseSnailfishInner (c:cs) stack
              | c == '[' || c == ',' = parseSnailfishInner cs stack
              | isDigit c = parseSnailfishInner cs (Simple (digitToInt c) : stack)
              | c /= ']' = parseSnailfishInner cs (Complex firstTop secondTop : stack)
              | otherwise = Complex firstTop secondTop : parseSnailfishInner cs (Complex firstTop secondTop : drop 2 stack)
                    where [secondTop, firstTop] = take 2 stack

addSnailfishes :: Snailfish -> Snailfish -> Snailfish
addSnailfishes = Complex

getDepth :: Snailfish -> Int
getDepth (Simple _) = 0
getDepth (Complex first second) = 1 + maximum [getDepth first, getDepth second]

hasGreaterThanElem :: Snailfish -> Int -> Bool
hasGreaterThanElem (Simple value) n = value > n
hasGreaterThanElem (Complex first second) n = hasGreaterThanElem first n || hasGreaterThanElem second n

explode :: Snailfish -> Snailfish
explode = undefined

isComplexPair :: Snailfish -> Bool
isComplexPair (Complex _ _) = True
isComplexPair (Simple _) = False

splittedLeft :: Int -> Int
splittedLeft value = floor $ toRational value / 2

splittedRight :: Int -> Int
splittedRight value = ceiling $ toRational value / 2

split :: Snailfish -> Snailfish
split (Complex l@(Simple lValue) r@(Simple rValue))
    | lValue > 9 = Complex (Complex (Simple $ splittedLeft lValue) (Simple $ splittedRight lValue)) r
    | rValue > 9 = Complex l (Complex (Simple $ splittedLeft rValue) (Simple $ splittedRight rValue))
    | otherwise = Complex l r
split (Complex l@(Complex _ _) r@(Simple rValue))
    | rValue > 9 = Complex l (Complex (Simple $ splittedLeft rValue) (Simple $ splittedRight rValue))
    | otherwise = Complex (split l) r
split (Complex l@(Simple lValue) r@(Complex _ _))
    | lValue > 9 = Complex (Complex (Simple $ splittedLeft lValue) (Simple $ splittedRight lValue)) r
    | otherwise = Complex l (split r)
split (Complex l r) = Complex (split l) (split r)

reduce :: Snailfish -> Snailfish
reduce snailfish
    | getDepth snailfish > 4 = reduce $ explode snailfish
    | hasGreaterThanElem snailfish 4 = reduce $ split snailfish
    | otherwise = snailfish