part1 = sum . map ((-) <$> maximum <*> minimum)

part2 = sum . map q
    where q row = sum [y `div` x | y <- row, x <- row, y > x, y `mod` x == 0]

main :: IO ()
main = do
    sheet <- fmap (map (map read . words) . lines) getContents

    print $ part1 sheet
    print $ part2 sheet
