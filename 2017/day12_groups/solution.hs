import Data.Char (isDigit, isSpace)
import qualified Data.Map as Map
import Data.List (group, sort)

find uf x
    | x == next = x
    | otherwise = find uf next
    where
        next = uf Map.! x
union uf (x, y) = Map.insert (find uf y) (find uf x) uf where
unionFind n = foldl union (Map.fromList [(k, k) | k <- [0..n-1]])
sets uf = map ((find uf) . snd) $ Map.toList uf

part1 n pipes = let s = sets $ unionFind n pipes
                in length $ filter ((==) (head s)) s

part2 n = length . group . sort . sets . unionFind n

main = do
    input <- getContents
    let records = lines $ filter ((||) <$> isDigit <*> isSpace) input
        n = length records
        pipes = concatMap (parsePipe . map read . words) records where
        parsePipe :: [Int] -> [(Int, Int)]
        parsePipe [p1] = []
        parsePipe (p1:p2:p2s) = (p1,p2) : (parsePipe (p1:p2s))

    print $ part1 n pipes
    print $ part2 n pipes
