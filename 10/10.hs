import qualified Data.Map as Map
import Data.List (group, sort, foldl')

data Parenthesis = LeftPar Char | RightPar Char
type Chunk = [Parenthesis]

data ProcessResult = Correct | Corrupt Char | Incomplete String

isCorrupted :: ProcessResult -> Bool
isCorrupted res = case res of
    (Corrupt _) -> True
    _ -> False

fromCorrupt :: ProcessResult -> Char
fromCorrupt (Corrupt c) = c

fromIncomplete :: ProcessResult -> String
fromIncomplete (Incomplete value) = value

isIncomplete :: ProcessResult -> Bool
isIncomplete res = case res of
    (Incomplete _) -> True
    _ -> False

matchingParentheses :: [(Char, Char)]
matchingParentheses = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

parSymbol :: Parenthesis -> Char
parSymbol (LeftPar p) = p
parSymbol (RightPar p) = p

areMatchingPar :: Parenthesis -> Parenthesis -> Bool    -- the order is important
areMatchingPar p p' = (parSymbol p, parSymbol p') `elem` matchingParentheses

parseParenthesis :: Char -> Parenthesis
parseParenthesis p = if p `elem` map fst matchingParentheses then LeftPar p else RightPar p

parseInput :: IO [Chunk]
parseInput = fmap (map (concatMap (map parseParenthesis . (:[]))) . lines) (readFile "10.in")

isLeftPar :: Parenthesis -> Bool
isLeftPar p = case p of
    LeftPar _ -> True
    _ -> False

processChunk :: Chunk -> [Parenthesis] -> ProcessResult
processChunk [] [] = Correct
processChunk [] stack = Incomplete (map parSymbol stack)
processChunk (p:ps) stack
    | isLeftPar p = processChunk ps (p : stack)
    | otherwise = if null stack
                    then Incomplete (map parSymbol stack)
                    else
                        if areMatchingPar (head stack) p
                            then processChunk ps (tail stack)
                            else Corrupt (parSymbol p)

syntaxErrorPoints :: Map.Map Char Int
syntaxErrorPoints = Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

syntaxErrorScore :: [Char] -> Int
syntaxErrorScore = foldl (\acc (c, l) -> acc + syntaxErrorPoints Map.! c * l) 0 . map (\c -> (head c, length c)) . group . sort

fstPart :: IO ()
fstPart = parseInput >>= print . syntaxErrorScore . map fromCorrupt . filter isCorrupted . map (`processChunk` [])

rightParFromLeft :: Char -> Char
rightParFromLeft p = snd $ head $ filter (\(l, r) -> l == p) matchingParentheses

autocompletePoints :: Map.Map Char Int
autocompletePoints = Map.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

autocompleteScore :: String -> Int
autocompleteScore = foldl' (\acc c -> acc * 5 + (autocompletePoints Map.! c)) 0

sndPart :: IO ()
sndPart = do
    parentheses <- parseInput
    let incomplete = filter isIncomplete $ map (`processChunk` []) parentheses
    let autoCompleteScores = map (autocompleteScore . map rightParFromLeft . fromIncomplete) incomplete
    print $ sort autoCompleteScores !! div (length autoCompleteScores) 2

