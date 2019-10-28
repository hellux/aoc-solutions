import Data.Function ((&))
import Data.List (elemIndex)
import Data.Maybe (fromJust)

data Dance = Spin Int
           | Exchange Int Int
           | Partner Char Char
             deriving (Show)

replace i c xs = let (bef, _:aft) = splitAt i xs
                 in bef ++ c:aft

spin n xs = let (bef, aft) = splitAt (length xs-n) xs
            in aft ++ bef
            
exchange i1 i2 xs = replace i1 (xs!!i2)
                  $ replace i2 (xs!!i1) xs

partner c1 c2 xs = exchange i1 i2 xs where
    i1 = fromJust $ elemIndex c1 xs
    i2 = fromJust $ elemIndex c2 xs

dance (Spin     n    ) = spin n
dance (Exchange i1 i2) = exchange i1 i2
dance (Partner  c1 c2) = partner c1 c2

parse :: String -> [Dance]
parse = map parseDance . words . map c2s where
    parseDance ('s':cs) = Spin (read cs)
    parseDance ('x':cs) = let (a, (_:b)) = break (== '/') cs
                          in Exchange (read a) (read b)
    parseDance ('p':c1:_:c2:[]) = Partner c1 c2
    c2s ',' = ' '
    c2s c = c

perform start = foldl (&) start . map dance
startLine = take 16 ['a'..]

part1 = perform startLine

main = do
    input <- getContents
    let dances = parse input
    print $ part1 dances
