import Data.Char (digitToInt, intToDigit)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

destination c cs = case elemIndex next cs of
    Just i -> i + 1
    Nothing -> destination next cs
    where next = mod (c-1) 10

move (c:s1:s2:s3:cs) = before ++ s1:s2:s3 : after ++ [c] where
    (before, after) = splitAt (destination c cs) cs

normalize cs = map intToDigit $ tail after ++ before where
    (before, after) = splitAt (fromJust $ elemIndex 1 cs) cs

part1 = normalize . (!! 100) . iterate move

main = do
    cups <- fmap (map digitToInt . init) getContents

    putStrLn $ part1 cups
