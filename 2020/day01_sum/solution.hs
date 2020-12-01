import Data.List (sort)

cmp des@((dp,ds):es) asc@(a:sc)
    | s == 2020 = dp * a
    | s  > 2020 = cmp es asc
    | s  < 2020 = cmp des sc
    where s = ds + a

-- O(nlogn) (sort)
part1 des asc = cmp [ (x, x) | x <- des ] asc

-- O(n^2) (square list comprehension)
part2 des asc = cmp [ (x*y, x+y) | x <- des, y <- des ] asc

main = do
    lines <- fmap lines getContents
    let ascending = sort $ map read lines
    let descending = reverse ascending

    print $ part1 descending ascending
    print $ part2 descending ascending
