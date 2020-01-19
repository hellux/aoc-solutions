import Data.List (sort, sortBy, groupBy)
import Data.Function (on)

import Data.Set (Set)
import qualified Data.Set as Set

type Component = (Int, Int)
type Components = Set Component
type Bridge = [Component]

parseComponent [a, b] = (a, b)
parseComponents :: String -> Components
parseComponents input = Set.fromList $ map parseLine $ lines input where
    parseLine = parseComponent . map read . words . map replace
    replace '/' = ' '
    replace c = c

connect :: Int -> Components -> [Bridge]
connect link comps = bridges1 ++ bridges2 where
    matching port = filter ((== link) . port) $ Set.toList comps
    branch port c = case branchingBridges of
                [] -> [[c]]
                bs -> map (c:) bs
        where branchingBridges = connect (port c) (Set.delete c comps)
    bridges1 = concatMap (branch snd) (matching fst)
    bridges2 = concatMap (branch fst) (matching snd)

strength :: Bridge -> Int
strength = sum . map (\(a, b) -> a + b)

part1 = last . sort . map strength
part2 = part1 . last
      . groupBy ((==) `on` length)
      . sortBy (compare `on` length)

main = do
    input <- getContents
    let comps = parseComponents input
    let bridges = connect 0 comps

    print $ part1 bridges
    print $ part2 bridges
