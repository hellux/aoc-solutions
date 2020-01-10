import Data.Char (isAsciiLower)
import Data.Map (Map)
import qualified Data.Map as Map

type Reg = Char
data Expr = Value Int | Register Reg deriving Show
data Instr = Snd Expr
           | Set Reg Expr
           | Add Reg Expr
           | Mul Reg Expr
           | Mod Reg Expr
           | Rcv Expr
           | Jgz Expr Expr
           deriving Show

type Program = [Instr]
type Memory = Map Char Int

data State = State { program :: Program
                   , pc :: Int
                   , memory :: Memory
                   , played :: [Int]
                   , recovered :: Maybe Int
                   } deriving Show

initialState prgm = State { program = prgm
                          , pc = 0
                          , memory = Map.empty
                          , played = []
                          , recovered = Nothing
                          }

parseExpr str@(c:_)
    | isAsciiLower c = Register c
    | otherwise = Value (read str)
parseInstr ["snd", x]        = Snd (parseExpr x)
parseInstr ["set", (x:_), y] = Set x (parseExpr y)
parseInstr ["add", (x:_), y] = Add x (parseExpr y)
parseInstr ["mul", (x:_), y] = Mul x (parseExpr y)
parseInstr ["mod", (x:_), y] = Mod x (parseExpr y)
parseInstr ["rcv", x]        = Rcv (parseExpr x)
parseInstr ["jgz", x, y]     = Jgz (parseExpr x) (parseExpr y)
parseProgram = map (parseInstr . words) . lines where

evalExpr :: Expr -> Memory -> Int
evalExpr (Value x) _ = x
evalExpr (Register x) mem = Map.findWithDefault 0 x mem

execute :: State -> State
execute state = if inside && recovered newState == Nothing
    then execute (newState { pc = newPc })
    else newState
    where instr = program state !! (pc state)
          newState = executeInstr instr state
          newPc = (pc newState + 1)
          inside = pc state < length (program state)
                && pc state >= 0

executeInstr :: Instr -> State -> State
executeInstr instr state = case instr of
    Snd x -> state { played = evalExpr x (memory state) : (played state) }
    Set x y -> let yval = evalExpr y (memory state)
               in state { memory = Map.insert x yval (memory state) }
    Add x y -> let yval = evalExpr y (memory state)
               in state { memory = Map.adjust (+yval) x (memory state) }
    Mul x y -> let yval = evalExpr y (memory state)
               in state { memory = Map.adjust (*yval) x (memory state) }
    Mod x y -> let yval = evalExpr y (memory state)
               in state { memory = Map.adjust (`mod` yval) x (memory state) }
    Rcv x -> if evalExpr x (memory state) /= 0
        then state { recovered = Just (head (played state)) }
        else state
    Jgz x y -> if evalExpr x (memory state) > 0
        then state { pc = pc state + evalExpr y (memory state) - 1 }
        else state

part1 = recovered . execute . initialState

main = do
    input <- getContents
    let prgm = parseProgram input

    print $ part1 prgm
