import Data.List (sort)

has_doublets (x:xs)
    | elem x xs = True
    | otherwise = has_doublets xs
has_doublets [] = False

part1 [] = 0
part1 (x:xs) = (if has_doublets x then 0 else 1) + part1 xs

{- check for doublets like part1 but sort characters in words first so anagrams
 - become equal -}
part2 phrases = valids $ map (map sort) phrases
    where valids (x:xs) = (if has_doublets x then 0 else 1) + valids xs
          valids [] = 0

main :: IO ()
main = do
    input <- getContents
    let passphrases = map words $ lines input

    print $ part1 passphrases
    print $ part2 passphrases
