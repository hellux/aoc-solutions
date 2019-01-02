import qualified Data.Map as Map
import Data.List (nub)

createRegs words_in = Map.fromList [(r, 0) | r <- regs] where
    regs = nub $ concat [[ireg, creg] | (ireg:_:_:_:creg:_) <- words_in]

createInstr words_in = [(i, ir, c, cr) |
        (ir:instr:ival:_:cr:cmp:cval:_) <- words_in,
        let i = flip (ifuncs Map.! instr) (read ival :: Int),
        let c = flip (cfuncs Map.! cmp) (read cval :: Int)]
    where
        ifuncs = Map.fromList [("inc", (+)), ("dec", (-))]
        cfuncs = Map.fromList [("==", (==)), ("<", (<)), ("<=", (<=)),
                               ("!=", (/=)), (">", (>)), (">=", (>=))]

maxRegValue regs = maximum [v | (_, v) <- Map.toList $ regs]

exec [] regs = regs
exec ((instr,ireg,cmp,creg):is) regs
    | cmp (regs Map.! creg) = exec is (Map.adjust instr ireg regs)
    | otherwise = exec is regs

execMax [] regs = maxRegValue regs
execMax ((instr,ireg,cmp,creg):is) regs
    | cmp (regs Map.! creg) = iter (Map.adjust instr ireg regs)
    | otherwise = iter regs
    where
        iter nextRegs = max (maxRegValue regs) (execMax is nextRegs)

part1 instr regs = maxRegValue (exec instr regs)
part2 = execMax

main :: IO ()
main = do
    input <- getContents
    let words_in = map words $ lines input
    let regs = createRegs words_in
    let instr = createInstr words_in

    print $ part1 instr regs
    print $ part2 instr regs
