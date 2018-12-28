import Data.List (sort)

part1 :: [[Int]] -> Int
part1 = sum . map (\row -> maximum row - minimum row)

part2 :: [[Int]] -> Int
part2 = sum . map (quotient . sort)
quotient :: [Int] -> Int
quotient (x:xs) = sum (map f xs) + quotient xs
    where f y = if mod y x == 0 then y `div` x else 0
quotient x = 0

main :: IO ()
main = do
    input <- getContents
    let sheet = (map (map (read::String->Int) . words) . lines) input

    print $ part1 sheet
    print $ part2 sheet
