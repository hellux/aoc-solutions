import Data.List (find)

{- spiraling sequence from given sequence [a,b,c,d..]:
 - [a,b,c,c,d,d,e,e,e,d,d,d...], where each value repeats 1 extra time every
 - other value -}
spiralseq seq = concat $ zipWith f [0..] $ concatMap (replicate 2) [1..]
                where f i x = replicate x (seq !! i)

spiralcum seq = scanl (+) 0 $ spiralseq $ seq

{-       x
 -    -1 0 1
 -
 - -1  5 4 3
 -y 0  6 1 2
 -  1  7 8 9 ...
 -
 - sum differences for coordinates to get final coordinate
 - e.g. x coordinate start at 0 and change by:
 -  i = [1, 2, 3, 4, 5, 6, 7, 8, 9..]
 -    dx = [1, 0,-1,-1, 0, 0, 1, 1..]
 -  cumulative sum:
 -  x = [0, 1, 1, 0,-1,-1,-1, 0, 1..] -}
spiral_x = spiralcum $ cycle [1, 0, -1, 0]
spiral_y = spiralcum $ cycle [0, 1, 0, -1]

spiral_manhattan i = abs (spiral_x !! (i-1)) + abs (spiral_y !! (i-1))
part1 = spiral_manhattan

{- value = sum of previous adjacent values
 - square 1 starts with value 1 -}
square_value :: Int -> Int
square_value 1 = 1
square_value i = sum $ map square_value (adjacent i)
    where adjacent i0 = [i | i <- [1..(i0-1)],
                        abs(spiral_x !! (i0-1) - spiral_x !! (i-1)) <= 1,
                        abs(spiral_y !! (i0-1) - spiral_y !! (i-1)) <= 1]
square_values = map square_value [1..]
part2 square = find (>square) square_values

main :: IO ()
main = do
    input <- getLine
    let square = read input :: Int

    print $ part1 square
    print $ part2 square
