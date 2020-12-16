import Data.List (sort, group)

part1 ds = cnt 1 * cnt 3 where
    cnt x = length $ filter (/= x) ds

combinations 4 = 7 -- can't jump 4
combinations n = 2^(n-1) -- valid for n <= 3
part2 = product . map (combinations . length) . filter ((== 1) . head) . group

main = do
    input <- getContents
    let adapters = sort $ map read $ words input
    let n = length adapters
    let rating = last adapters + 3
    let (lower, higher) = (0 : adapters, adapters ++ [rating])
    let diffs = zipWith (-) higher lower

    print $ part1 diffs
    print $ part2 diffs
