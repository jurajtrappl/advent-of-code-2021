type Depth = Int

parseInput :: IO [Depth]
parseInput = do
    fContent <- readFile "01.in"
    return $ map (\x -> read x :: Depth) $ lines fContent

windowsOf :: Int -> [a] -> [[a]]
windowsOf n (x:xs)
    | length xs >= n = (x : take (n - 1) xs) : windowsOf n xs
    | otherwise = [x:xs]

depthSweep :: [Depth] -> Int
depthSweep = length . filter (\l -> head l < last l) . windowsOf 2

fstPart :: IO ()
fstPart = parseInput >>= (print . depthSweep)

sndPart :: IO ()
sndPart = parseInput >>= (print . depthSweep . map sum . windowsOf 3)

