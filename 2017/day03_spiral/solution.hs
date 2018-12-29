spiralseq sequence =
    foldr (++) [] $ map f (zip [0..] [div a 2 + mod a 2 | a <- [1..]])
    where f (i, x) = take x $ repeat $ sequence !! (i `mod` length sequence)
spiralseq_x = spiralseq [1, 0, -1, 0]
spiralseq_y = spiralseq [0, 1, 0, -1]
spiral_x i = sum $ take (i-1) spiralseq_x
spiral_y i = sum $ take (i-1) spiralseq_y

spiral_manhattan i = abs (spiral_x i) + abs (spiral_y i)
part1 = spiral_manhattan


square_value :: Int -> Int
square_value 1 = 1
square_value i = sum $ map square_value (adjacent i)
    where adjacent i0 = [i | i <- [1..(i0-1)],
                        abs(spiral_x i0 - spiral_x i) <= 1,
                        abs(spiral_y i0 - spiral_y i) <= 1]
square_values = map square_value [1..]
part2 square = head [x | x <- square_values, x > square]

main :: IO ()
main = do
    input <- getLine
    let square = read input :: Int

    print $ part1 square
    print $ part2 square
