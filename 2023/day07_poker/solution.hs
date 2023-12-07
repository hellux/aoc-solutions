import Data.List (sort, sortOn, group)
import Data.Char (digitToInt)

parseInput = map parseLine . filter (not . null) . lines where
    parseLine = (\[c, b] -> (map t c, read b :: Int)) . words
    t 'T' = 10
    t 'J' = 11
    t 'Q' = 12
    t 'K' = 13
    t 'A' = 14
    t c = digitToInt c

kind = t . distr where
    distr = reverse . sortOn snd . map (\x -> (head x, length x)) . group . sort
    t ((a, 5):_)        = 6 -- five of a kind
    t ((a, 4):_)        = 5 -- four of a kind
    t ((a, 3):(b, 2):_) = 4 -- full house
    t ((a, 3):_)        = 3 -- three of a kind
    t ((a, 2):(b, 2):_) = 2 -- two pair
    t ((a, 2):_)        = 1 -- one pair
    t _                 = 0 -- high card

value = sum . zipWith (\i x -> 15^i * x) [1..] . reverse
key x = (kind x, value x)

part1 = sum . zipWith (\rank (_, bid) -> bid * rank) [1..] . sortOn (key . fst)

main = do
    input <- fmap parseInput getContents
    print $ part1 input
