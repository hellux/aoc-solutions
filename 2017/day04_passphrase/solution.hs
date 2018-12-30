import Data.List (sort)

only_unique :: Eq a => [a] -> Bool
only_unique [] = True
only_unique (x:xs)
    | elem x xs = False
    | otherwise = only_unique xs

part1 = length . (filter only_unique)
part2 = part1 . map (map sort)

main :: IO ()
main = do
    input <- getContents
    let passphrases = map words $ lines input

    print $ part1 passphrases
    print $ part2 passphrases
