import qualified Data.Map as Map
import Data.List (sortBy, groupBy, find)
import Data.Function (on)
import Data.Maybe (fromJust)

{- Tower data structure -}

parseAbove [prgm, _] = (prgm, [])
parseAbove (prgm:_:_:carries) = (prgm, map (filter (\x -> x /= ',')) carries)
parseBelow [prgm, _] = []
parseBelow (prgm:_:_:carries) = [(c, prgm) |
                                 c <- map (filter (\x -> x /= ',')) carries]
parseWeight (prgm:weight:_) = (prgm, read weight :: Int)

createTower input =
    Map.fromList [(k, (b, w , a)) | (k, _) <- Map.toList towerWeight,
                  let b = Map.lookup k towerBelow,
                  let w = towerWeight Map.! k,
                  let a = towerAbove Map.! k] where
    towerAbove = Map.fromList $ map (parseAbove . words) $ lines input
    towerBelow = Map.fromList $ concat $ map (parseBelow . words) $ lines input
    towerWeight = Map.fromList $ map (parseWeight . words) $ lines input

below  prgm tower = b where (b, _, _) = tower Map.! prgm
weight prgm tower = w where (_, w, _) = tower Map.! prgm
above  prgm tower = a where (_, _, a) = tower Map.! prgm

{- Solutions -}

bottomProgram tower = btmPrgmDsc (fst (Map.toList tower !! 0)) tower where
    btmPrgmDsc prgm tower = case below prgm tower of
        Just p -> btmPrgmDsc p tower
        Nothing -> prgm

part1 = bottomProgram

weightTower prgm tower =
    weight prgm tower + sum [weightTower p tower | p <- above prgm tower]

unbalanced prgm tower = pat grps where
    grps = let cmp = compare `on` (`weightTower` tower)
           in sortBy (compare `on` length) $
              groupBy (\x y -> cmp x y == EQ) $
              sortBy cmp (above prgm tower) 
    pat [] = Nothing
    pat [bal] = Just prgm
    pat ([unbal]:_) = unbalanced unbal tower

part2 tower = (weight unbal tower) + error where
    unbal = fromJust $ unbalanced (bottomProgram tower) tower
    bal = fromJust $ find ((/=) unbal) $
          above (fromJust $ below unbal tower) tower
    error = (weightTower bal tower) - (weightTower unbal tower)

main :: IO ()
main = do
    input <- getContents
    let tower = createTower input

    print $ part1 tower
    print $ part2 tower
