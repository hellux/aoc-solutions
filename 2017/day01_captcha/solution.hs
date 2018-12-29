import Data.Char (digitToInt)

checksum :: [Int] -> Int -> Int -> Int
checksum (x:xs) i skip
    | i <= 0                = 0
    | x == xs !! (skip-1)   = x + checksum xs (i-1) skip
    | otherwise             = checksum xs (i-1) skip

part1 :: [Int] -> Int
part1 xs = checksum (cycle xs) (length xs) (1)

part2 :: [Int] -> Int
part2 xs = checksum (cycle xs) (length xs) ((length xs) `div` 2)

main :: IO ()
main = do 
    input <- getLine
    let list = map digitToInt input

    print $ part1 list
    print $ part2 list
