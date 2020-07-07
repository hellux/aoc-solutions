import Data.List (group)

runLength = concat . map label . group where
    label g = (show $ length g) ++ [(head g)]

runLengthN n = length . last . take (n+1) . iterate runLength

part1 = runLengthN 40
part2 = runLengthN 50

main = do
    input <- getContents

    print $ part1 input
    print $ part2 input
