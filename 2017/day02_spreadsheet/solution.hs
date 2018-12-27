part1 = sum
    . map ((\row -> maximum row - minimum row)
    . map (read::String->Int) . words)
    . lines

main :: IO ()
main = interact $ show . part1
