import Data.List (groupBy, transpose)
import Data.Char (isDigit)

parseInput = map (\[x,y] -> (x,y))
        . transpose
        . map (map read . filter (isDigit . head)
        . groupBy (\x y -> isDigit x == isDigit y)
        . dropWhile (' ' ==)
        . drop 10)
    . filter (not . null)
    . lines

-- solve v(v-t) > d for v
bounds (t, d) = (floor $ (tf - s) / 2 + 1, ceiling $ (tf + s) / 2 - 1) where
    tf = fromIntegral t :: Float
    df = fromIntegral d :: Float
    s = sqrt (tf*tf - 4*df)

race = (\(l,u) -> u-l+1) . bounds

part1 = product . map race

part2 input = race (t, d) where
    [t, d] = map (read . concatMap show) $ transpose $ map (\(x,y) -> [x,y]) input

main = do
    input <- fmap parseInput getContents
    print $ part1 input
    print $ part2 input
