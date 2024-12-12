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

part1 rules = sum . map (\xs -> xs !! (div (length xs) 2)) . filter isSorted where
    cmp a b = if elem (a, b) rules then LT else GT
    isSorted ls = sortBy cmp ls == ls

main = do
    (rules, pages) <- fmap (parse . lines) getContents
    print $ part1 rules pages
