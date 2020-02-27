import Data.Char (digitToInt, intToDigit)

row signal n = (`mod` 10)
             $ abs
             $ sum
             $ zipWith (*) signal pattern where
    pattern = tail
            $ cycle
            $ concatMap (replicate n) [0, 1, 0, -1]

fft signal = map (row signal) [1.. length signal] 

part1 = map intToDigit
      . take 8
      . last
      . take (100+1)
      . iterate fft
      . map digitToInt

main = do
    signal <- getContents
    putStrLn $ part1 signal
