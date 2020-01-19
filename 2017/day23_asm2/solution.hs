import Data.Char (isAsciiLower)
import Data.Map (Map)
import qualified Data.Map as M

type Reg = Char
data Expr = Value Int | Register Reg deriving Show
data Instr = Set Reg Expr
           | Sub Reg Expr
           | Mul Reg Expr
           | Jnz Expr Expr
           deriving Show

type Program = [Instr]
type Memory = Map Char Int

data State = State { program :: Program
                   , pc :: Int
                   , memory :: Memory
                   } deriving Show

initialState debug prgm = State { program = prgm
                                , pc = 0
                                , memory = M.singleton 'a' aVal
                                }
    where aVal = if debug then 0 else 1

parseExpr str@(c:_)
    | isAsciiLower c = Register c
    | otherwise = Value (read str)
parseInstr ["set", (x:_), y] = Set x (parseExpr y)
parseInstr ["sub", (x:_), y] = Sub x (parseExpr y)
parseInstr ["mul", (x:_), y] = Mul x (parseExpr y)
parseInstr ["jnz", x, y]     = Jnz (parseExpr x) (parseExpr y)
parseProgram = map (parseInstr . words) . lines where

reg = M.findWithDefault 0

evalExpr :: Expr -> Memory -> Int
evalExpr (Value x) _ = x
evalExpr (Register x) mem = reg x mem

executeInstr :: Instr -> State -> State
executeInstr instr st' = case instr of
    Set x y -> let yval = evalExpr y (memory st)
               in st { memory = M.insert x yval (memory st) }
    Sub x y -> let yval = evalExpr y (memory st)
               in st { memory = M.adjust (subtract yval) x (memory st) }
    Mul x y -> let yval = evalExpr y (memory st)
               in st { memory = M.adjust (*yval) x (memory st) }
    Jnz x y -> if evalExpr x (memory st) /= 0
        then st { pc = pc st' + evalExpr y (memory st) }
        else st
    where st = st' { pc = pc st' + 1 }

part1 = execute . initialState True where
    execute st = let instr = program st !! (pc st)
                     newState = executeInstr instr st
                     inside = pc newState < length (program newState)
                           && pc newState >= 0
                     mul = case instr of Mul _ _ -> 1; _ -> 0
                 in if inside then execute newState + mul else mul

isPrime :: Int -> Bool
isPrime n = factors == [] where
    potentials = 2 : [3, 5.. round $ sqrt $ fromIntegral n]
    factors = filter (((==) 0) . mod n) potentials

part2 prgm = length $ filter (not . isPrime) [start, start+jump.. end] where
    Set 'b' (Value b0)    = prgm !! 0
    Sub 'c' (Value range) = prgm !! 7
    Sub 'b' (Value bd)    = prgm !! 30
    start = 100*b0 + 100000
    end = start-range
    jump = negate bd

main = do
    input <- getContents
    let prgm = parseProgram input

    print $ part1 prgm
    print $ part2 prgm
