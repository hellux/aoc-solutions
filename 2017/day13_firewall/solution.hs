import Data.Char (isNumber)
import Data.List (elemIndex)

collides delay (depth, range) =
    let scannerPos = [0..range-1] ++ [range-2, range-3..1]
    in (scannerPos) !! mod (depth+delay) (2*range-2) == 0

part1 = sum . map prod where
    prod l@(depth, range)
        | collides 0 l = depth*range
        | otherwise = 0

part2 layers = elemIndex False [or $ map (collides d) layers | d <- [0..]]

main = do
    input <- fmap (map (map (read . filter isNumber) . words) . lines) getContents
    let layers = [(depth, range) | [depth, range] <- input]

    print $ part1 layers
    print $ part2 layers
