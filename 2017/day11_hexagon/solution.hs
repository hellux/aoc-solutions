import Data.Char (toUpper)
import Data.Complex
import Data.List (minimumBy, maximum)
import Data.Function (on)

data Dir = N | NE | SE | S | SW | NW deriving (Read, Show, Enum)

{- y axis scaled s.t. 1 corresponds to sqrt(3) in actual grid -}
dir N  = ( 0) :+ ( 2)
dir NE = ( 2) :+ ( 1)
dir SE = ( 2) :+ (-1)
dir S  = ( 0) :+ (-2)
dir SW = (-2) :+ (-1)
dir NW = (-2) :+ ( 1)

step pos d = pos + (dir d)
walk = foldl step 0

distance 0 = 0
distance pos = 1 + distance next where
    next = minimumBy (compare `on` (realPart . abs)) $ map (step pos) [N .. NW]

part1 = distance . walk
part2 = maximum . map distance . scanl step 0

main = do
    input <- getLine
    let dirs = map read $ words $ map (toUpper . comspc) input where
        comspc ',' = ' '
        comspc c = c

    print $ part1 dirs
    print $ part2 dirs
