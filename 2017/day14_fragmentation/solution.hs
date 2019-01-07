import Data.Char (ord)
import Data.Bits (xor, (.&.), shiftR)
import qualified Data.Map as Map
import Data.List (group, sort)

revsec xs i len
    | i+len <= (length xs) = take i xs ++
                             (reverse $ take len $ drop i xs) ++
                             drop (i+len) xs
    | otherwise = let e = len-(length xs-i)
                      rev = reverse $ drop i xs ++ take e xs
                  in drop (len-e) rev ++
                     take (i-e) (drop e xs) ++
                     take (len-e) rev
sparse input = sIter [] [0..255] 0 0 64 where
    lengths = (map ord input) ++ [17, 31, 73, 47,23]
    sIter []     xs i skip 0      = xs
    sIter []     xs i skip rounds = sIter lengths xs i skip (rounds-1)
    sIter (l:ls) xs i skip rounds = sIter ls
                                          (revsec xs i l)
                                          (mod (i+l+skip) (length xs))
                                          (skip+1)
                                          rounds
knotHash :: String -> [Bool]
knotHash = concatMap (toBool 8) . xor16 . sparse where
    xor16 [] = []
    xor16 xs = foldl1 xor (take 16 xs) : xor16 (drop 16 xs)
    toBool :: Int -> Int -> [Bool]
    toBool 0 _ = []
    toBool size n = (toBool (size-1) (shiftR n 1)) ++ [(n .&. 1) == 1]

grid :: String -> Map.Map (Int, Int) Bool
grid key = Map.fromList $ concat
            [zip [(x, y) | x <- [0..]] hash | y <- [0..127],
                let hash = knotHash (key ++ "-" ++ (show y))]

part1 = Map.size . Map.filter ((==) True) . grid

find uf k = case Map.lookup k uf of
                Nothing -> k
                Just next -> if next == k then k else find uf next
union uf (k1, k2) = Map.insert root2 root2 $ Map.insert root1 root2 uf where
    root1 = find uf k1 
    root2 = find uf k2
sets uf = map ((find uf) . snd) $ Map.toList uf
setCount = length . group . sort . sets

regions g = setCount $ Map.foldrWithKey combine Map.empty g where
    combine pos@(x,y) occupied uf =
        if (occupied)
            then foldl union uf [(pos, neigh) | neigh <- inRegion]
            else uf
        where
            adjacent = [(x-1,y), (x,y-1)]
            inRegion = pos:filter (\p -> Map.lookup p g == Just True) adjacent

part2 = regions . grid

main = do
    key <- getLine

    print $ part1 key
    print $ part2 key
