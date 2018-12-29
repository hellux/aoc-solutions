import qualified Data.Sequence as S

jump jmps i instr change
    | 0 <= i && i < S.length instr =
        jump (jmps+1)
        (i + S.index instr i)
        (S.update i (change (S.index instr i)) instr)
        change
    | otherwise = jmps

part1 jumps = jump 0 0 (S.fromList jumps) succ
part2 jumps = jump 0 0 (S.fromList jumps) change
    where change x | x >= 3 = x-1 | otherwise = x+1

main :: IO ()
main = do
    input <- getContents
    let jumps = map (read::String->Int) (lines input)

    print $ part1 jumps
    print $ part2 jumps
