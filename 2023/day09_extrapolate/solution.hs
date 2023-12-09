parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

next x
    | all (== l) x = l
    | otherwise = l + next (diffs x)
    where l = last x

diffs x = zipWith (-) (tail x) x

part1 = sum . map next

main = do
    input <- fmap parseInput getContents
    print $ part1 input
