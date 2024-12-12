import Data.List (elemIndex, sortBy)

splitOn c x = case elemIndex c x of
    Nothing -> [x]
    Just i -> let (y, ys) = splitAt i x
              in y : splitOn c (drop 1 ys)

parse :: [String] -> ([(Int, Int)], [[Int]])
parse ls = ( map parseRule (filter (elem '|') ls)
           , map parsePage (filter (elem ',') ls)
           ) where
    parseRule = (\[a,b] -> (a, b)) . map read . splitOn '|'
    parsePage = map read . splitOn ','

cmp rules a b = if elem (a, b) rules then LT else GT
middle xs = xs !! (div (length xs) 2)
isSorted rules ls = sortBy (cmp rules) ls == ls

part1 rules = sum . map middle . filter (isSorted rules)
part2 rules = sum . map (middle . sortBy (cmp rules)) . filter (not . (isSorted rules))

main = do
    (rules, pages) <- fmap (parse . lines) getContents
    print $ part1 rules pages
    print $ part2 rules pages
