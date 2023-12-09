parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

next x
    | all (== l) x = l
    | otherwise = l + next (diffs x)
    where l = last x

prev x
    | all (== l) x = l
    | otherwise = l - prev (diffs x)
    where l = head x

diffs x = zipWith (-) (tail x) x

part1 = sum . map next
part2 = sum . map prev

main = do
    input <- fmap parseInput getContents
    print $ part1 input
    print $ part2 input
