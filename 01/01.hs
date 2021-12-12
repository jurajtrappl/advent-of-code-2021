type Depth = Int

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

parseInput :: IO [Depth]
parseInput = fmap (map unsafeReadInt . lines) (readFile "01.in")

windowsOf :: Int -> [a] -> [[a]]
windowsOf size (x:xs)
    | length xs >= size = (x : take (size - 1) xs) : windowsOf size xs
    | otherwise = [x:xs]

depthSweep :: [Depth] -> Int
depthSweep = length . filter (\l -> head l < last l) . windowsOf 2

fstPart :: IO ()
fstPart = parseInput >>= (print . depthSweep)

sndPart :: IO ()
sndPart = parseInput >>= (print . depthSweep . map sum . windowsOf 3)

