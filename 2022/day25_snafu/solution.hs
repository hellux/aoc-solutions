import Data.Char (intToDigit, digitToInt)
import Control.Monad.State

base = 5
weights = [base^i | i <- [0..]]

fromSnafu = sum . zipWith (*) weights . reverse . map sd where
    sd '=' = -2
    sd '-' = -1
    sd x = digitToInt x

toBase x = map (`rem` base) $ takeWhile (> 0) $ map (div x) weights

toSnafu = str . fst . (`runState` False) . mapM wc . (++ [0]) . toBase where
    wc :: Int -> State Bool Int
    wc x = do
        carry_prev <- get
        let y = if carry_prev then x + 1 else x
        let carry = y >= (base - 2)
        put carry
        return y
    ds x
        | x == base = '0'
        | x == base - 1 = '-'
        | x == base - 2 = '='
        | otherwise = intToDigit x
    str = map ds . dropWhile (== 0) . reverse

part1 = toSnafu . sum . map fromSnafu

main = do
    input <- fmap lines getContents

    putStrLn $ part1 input
