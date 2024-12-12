import Data.List.Split (splitOn)
import Data.List (transpose, find)
import Data.Maybe (fromJust)

horiRefl rows = find r [1..length rows-1] where
    r i = let (a, b) = splitAt i rows
          in and $ zipWith (==) (reverse a) b
vertRefl = horiRefl . transpose

p1 rows = maybe (fromJust $ vertRefl rows) (*100) (horiRefl rows)
part1 = sum . map p1

main = do
    input <- fmap (splitOn [""] . lines) getContents
    print $ part1 input