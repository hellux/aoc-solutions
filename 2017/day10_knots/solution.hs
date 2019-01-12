import Data.Char (ord)
import Text.Printf (printf)
import Data.Bits (xor)

{- reverse sequence of string
 - case 1: no wrap of sequenc
 - 0   i     e = i+len
 - xxxxrrrrrrXXXX
 - case 2: sequence wraps
 - 0  e       i
 - rrrxxxxxxxxRRR
 - RRRrrr (rev)
 -}
revsec xs i len
    | i+len <= (length xs) = take i xs ++
                             (reverse $ take len $ drop i xs) ++
                             drop (i+len) xs
    | otherwise = let e = len-(length xs-i)
                      rev = reverse $ drop i xs ++ take e xs
                  in drop (len-e) rev ++
                     take (i-e) (drop e xs) ++
                     take (len-e) rev

tie lengths = tieIter lengths [0..255] 0 0 where
    tieIter []     xs i skip = xs
    tieIter (l:ls) xs i skip = tieIter ls
                                       (revsec xs i l)
                                       (mod (i+l+skip) (length xs))
                                       (skip+1)

part1 = foldl1 (*) . take 2 . tie

sparse input = sIter [] [0..255] 0 0 64 where
    lengths = (map ord input) ++ [17, 31, 73, 47,23]
    sIter []     xs i skip 0      = xs
    sIter []     xs i skip rounds = sIter lengths xs i skip (rounds-1)
    sIter (l:ls) xs i skip rounds = sIter ls
                                          (revsec xs i l)
                                          (mod (i+l+skip) (length xs))
                                          (skip+1)
                                          rounds

dense = concat . map hex . xor16 . sparse where
    hex = printf "%02x" :: Int->String
    xor16 [] = []
    xor16 xs = foldl1 xor (take 16 xs) : xor16 (drop 16 xs)

part2 = dense

main = do
    input <- getContents
    let lengths = map read $ words $ map comspc input where
        comspc ',' = ' '
        comspc c = c

    print $ part1 lengths
    putStrLn $ part2 input
