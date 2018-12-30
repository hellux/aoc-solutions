import qualified Data.Sequence as Seq

jump change jmps i instr
    | 0 <= i && i < Seq.length instr =
        jump change (jmps+1) (i + Seq.index instr i)
        (Seq.update i (change (Seq.index instr i)) instr)
    | otherwise = jmps

part1 jumps = jump succ 0 0 (Seq.fromList jumps)
part2 jumps = jump change 0 0 (Seq.fromList jumps)
    where change x | x >= 3 = x-1 | otherwise = x+1

main :: IO ()
main = do
    input <- getContents
    let jumps = map (read::String->Int) (lines input)

    print $ part1 jumps
    print $ part2 jumps
