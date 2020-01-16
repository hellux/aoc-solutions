import Data.List (transpose, nub, sort)

type Square = [[Bool]]
type Grid = [[Bool]]
type Squares = [[Square]]
data Rule = Rule Square Square deriving (Eq, Show, Ord)

parseSquare = map (map replace) . split where
    split = words . map (\c -> if c == '/' then ' ' else c)
    replace '.' = False
    replace '#' = True
parseRule [start, "=>", end] = nub $ sort (flipped ++ rotations) where
    unrotated = Rule (parseSquare start) (parseSquare end)
    rotateRight = map reverse . transpose
    rotateRule (Rule a b) = Rule (rotateRight a) b
    flipRule (Rule a b) = Rule (reverse a) b
    rotations = take 4 $ iterate rotateRule unrotated
    flipped = map flipRule rotations
parseRules = concatMap (parseRule . words) . lines

chunksOf _ [] = []
chunksOf n xs = (take n xs) : chunksOf n (drop n xs)
reorder :: Int -> [a] -> [a]
reorder n = concat . transpose . chunksOf n 

divide :: Grid -> Squares
divide ss = transpose 
          $ chunksOf w
          $ chunksOf n
          $ reorder w
          $ chunksOf n
          $ concat
          $ ss where 
    s = length ss                       -- image width in pixels
    n = if s `mod` 2 == 0 then 2 else 3 -- square width
    w = s `div` n                       -- image width in squares

join :: Squares -> Grid
join ss = chunksOf s
        $ concat
        $ reorder s
        $ concat
        $ concat
        $ transpose
        $ ss where
    w = length ss
    s = length (ss!!0!!0) * w

match :: Square -> [Rule] -> Square
match sq = end . head . filter matches where
    matches (Rule start end) = sq == start
    end (Rule _ end) = end

enhance :: [Rule] -> Grid -> Grid
enhance rules = join . map (map (`match` rules)) . divide
enhanceN rules n = last . take (n+1) . iterate (enhance rules)

lit = length . filter id . concat

startImage = [ [False, True,  False]
             , [False, False, True]
             , [True,  True,  True]
             ]

part1 rules = lit . enhanceN rules 5
part2 rules = lit . enhanceN rules 18

main = do
    rules <- fmap parseRules getContents
    print $ part1 rules startImage
    print $ part2 rules startImage
