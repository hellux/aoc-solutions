import Data.Char (digitToInt)

checksum :: [Int] -> Int -> Int -> Int
checksum (x:xs) i skip =
    if i <= 0 then 0
    else 
        if x == xs !! (skip-1)
            then x + checksum xs (i-1) skip
            else checksum xs (i-1) skip

checksum1 :: [Int] -> Int
checksum1 xs = checksum (cycle xs) (length xs) (1)

checksum2 :: [Int] -> Int
checksum2 xs = checksum (cycle xs) (length xs) ((length xs) `div` 2)

main :: IO ()
main = do 
    input <- getLine
    let list = map digitToInt input

    print $ checksum1 list
    print $ checksum2 list
