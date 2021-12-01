type Depth = Int

parseInput :: IO [Depth]
parseInput = do
    fContent <- readFile "01.in"
    return $ map (\x -> read x :: Depth) $ lines fContent

depthSweep :: [Depth] -> Int
depthSweep [x] = 0
depthSweep (x:rest@(y:ys))
    | x < y = 1 + depthSweep rest
    | otherwise = 0 + depthSweep rest

fstPart :: IO ()
fstPart = do
    input <- parseInput
    print $ depthSweep input

windowsOf :: Int -> [a] -> [[a]]
windowsOf n (x:xs)
    | length xs >= n = (x : take (n - 1) xs) : windowsOf n xs
    | otherwise = [x:xs]

sndPart :: IO ()
sndPart = do
    input <- parseInput
    print $ depthSweep $ map sum $ windowsOf 3 input

