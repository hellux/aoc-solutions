import Data.Bits ((.&.))
import Data.Function (on)
import Text.Printf (printf)

--import Numeric (showIntAtBase)
--binary = flip (showIntAtBase 2 (\x -> if x == 1 then '1' else '0')) ""

dividend = 2147483647
n = 40000000
aMultiplier = 16807
bMultiplier = 48271

lowN n = (.&.) (2^n-1)
generator m prev = (prev*m) `rem` dividend
cmp = (==) `on` (lowN 16)

part1 a b = do
    let generate n0 m = take n $ drop 1 $ iterate (generator m) n0
    let as = generate a aMultiplier
    let bs = generate b bMultiplier
    let matches = zipWith cmp as bs
    let numMatching = length $ filter id matches
    printf "Part 1: %d matching pairs\n" numMatching

parse :: String -> (Int, Int)
parse string = let [a, b] = (map (read . last . words) . lines) string
               in (a, b)

main = do
    input <- getContents
    let (a, b) = parse input

    part1 a b
