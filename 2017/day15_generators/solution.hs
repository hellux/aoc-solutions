import Data.Bits ((.&.))
import Data.Function (on)
import Text.Printf (printf)

--import Numeric (showIntAtBase)
--binary = flip (showIntAtBase 2 (\x -> if x == 1 then '1' else '0')) ""

dividend = 2147483647
n = 40000000
aMultiplier = 16807
bMultiplier = 48271

generator m prev = (prev*m) `rem` dividend
generate n0 m = take n $ drop 1 $ iterate (generator m) n0

lowN n = (.&.) (2^n-1)
cmp = (==) `on` (lowN 16)
countMatches as bs = length $ filter id $ zipWith cmp as bs

part1 a b = do
    let as = generate a aMultiplier
    let bs = generate b bMultiplier
    printf "Part 1: %d matching pairs\n" (countMatches as bs)

part2 a b = do
    let as = filter ((==0) . (`rem` 4)) $ generate a aMultiplier
    let bs = filter ((==0) . (`rem` 8)) $ generate b bMultiplier
    printf "Part 2: %d matching pairs\n" (countMatches as bs)

parse :: String -> (Int, Int)
parse string = let [a, b] = (map (read . last . words) . lines) string
               in (a, b)

main = do
    input <- getContents
    let (a, b) = parse input

    part1 a b
    part2 a b
