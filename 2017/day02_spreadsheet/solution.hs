import Data.List (sort)

part1 :: [[Int]] -> Int
part1 = sum . map (\row -> maximum row - minimum row)

part2 :: [[Int]] -> Int
part2 = sum . map q
    where q row = sum [y `div` x | y <- row, x <- row, y > x, y `mod` x == 0]

main :: IO ()
main = do
    input <- getContents
    let sheet = (map (map (read::String->Int) . words) . lines) input

    print $ part1 sheet
    print $ part2 sheet
