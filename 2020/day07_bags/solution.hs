import Data.List (elemIndex, find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

type Bag = String
type Inside = [(Int, Bag)]
type Rule = (Bag, Inside)
type Rules = Map Bag Inside

myBag = "shiny gold"

splitOnComma x = case elemIndex ',' x of
    Nothing -> [x]
    Just i -> let (y, ys) = splitAt i x
              in y : splitOnComma (drop 1 ys)

parseRule :: [String] -> Rule
parseRule (col1:col2:"bags":"contain":inside) =
    (unwords [col1, col2], parseBags (unwords inside))
    where
    parseBag [count, col1, col2, _] = Just (read count, unwords [col1, col2])
    parseBag _ = Nothing
    parseBags = mapMaybe (parseBag . words) . splitOnComma

canContain :: Rules -> Bag -> Bag -> Bool
canContain rules inner outer =
    elem inner inside || any (canContain rules inner) inside
    where inside = map snd $ rules M.! outer

part1 rules = length
            $ filter id
            $ map (canContain rules myBag)
            $ M.keys rules

totalBags :: Bag -> Rules -> Int
totalBags bag rules = totalBags' (rules M.! bag) where
    totalBags' x = 1 + sum (map g x)
    g (c, bag) = c * totalBags bag rules

part2 = pred . totalBags myBag

main = do
    input <- fmap lines getContents

    let rules = M.fromList $ map (parseRule . words) input

    print $ part1 rules
    print $ part2 rules
