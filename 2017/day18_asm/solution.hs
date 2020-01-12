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
                   , sendCount :: Int
                   , halt :: Bool
                   , wait :: Bool
                   } deriving Show

data Queues = Queues { queueSnd :: [Int]
                     , queueRcv :: [Int]
                     } deriving Show

type World = (State, Queues)

initialQueues = Queues { queueSnd = []
                       , queueRcv = []
                       }

initialState id prgm = State { program = prgm
                             , pc = 0
                             , memory = Map.singleton 'p' id
                             , played = []
                             , recovered = Nothing
                             , sendCount = 0
                             , halt = False
                             , wait = False
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

executeInstr1 :: Instr -> State -> State
executeInstr1 instr st' = case instr of
    Snd x -> st { played = evalExpr x (memory st) : (played st) }
    Set x y -> let yval = evalExpr y (memory st)
               in st { memory = Map.insert x yval (memory st) }
    Add x y -> let yval = evalExpr y (memory st)
               in st { memory = Map.adjust (+yval) x (memory st) }
    Mul x y -> let yval = evalExpr y (memory st)
               in st { memory = Map.adjust (*yval) x (memory st) }
    Mod x y -> let yval = evalExpr y (memory st)
               in st { memory = Map.adjust (`mod` yval) x (memory st) }
    Rcv x -> if evalExpr x (memory st) /= 0
        then st { halt = True, recovered = Just (head (played st)) }
        else st
    Jgz x y -> if evalExpr x (memory st) > 0
        then st { pc = pc st + evalExpr y (memory st) - 1 }
        else st
    where st = st' { pc = pc st' + 1 }

executeInstr2 :: Instr -> World -> World
executeInstr2 instr (st', qs) = case instr of
    Snd x -> let xval = evalExpr x (memory st)
                 newState = st { sendCount = sendCount st + 1 }
             in (newState, qs { queueSnd = (queueSnd qs) ++ [xval] })
    Set x y -> let yval = evalExpr y (memory st)
               in (st { memory = Map.insert x yval (memory st) }, qs)
    Add x y -> let yval = evalExpr y (memory st)
               in (st { memory = Map.adjust (+yval) x (memory st) }, qs)
    Mul x y -> let yval = evalExpr y (memory st)
               in (st { memory = Map.adjust (*yval) x (memory st) }, qs)
    Mod x y -> let yval = evalExpr y (memory st)
                   newMem = Map.adjust (`mod` yval) x (memory st)
               in (st { memory = newMem }, qs)
    Rcv x -> case queueRcv qs of
                (e:es) -> let Register r = x
                              newMem = Map.insert r e (memory st)
                              newQs = qs { queueRcv = es }
                          in (st { wait = False, memory = newMem }, newQs)
                _ -> (st' { wait = True }, qs)
    Jgz x y -> if evalExpr x (memory st) > 0
        then (st { pc = pc st + evalExpr y (memory st) - 1 }, qs)
        else (st, qs)
    where st = st' { pc = pc st' + 1 }

part1 = recovered . execute . initialState 0 where
    execute :: State -> State
    execute st = if inside && not (halt newState)
        then execute newState
        else newState
        where instr = program st !! (pc st)
              newState = executeInstr1 instr st
              inside = pc newState < length (program newState)
                    && pc newState >= 0

part2 prgm = sendCount $ (\(_, _, st2) -> st2) $ execute initial  where
    initial = (initialState 0 prgm, initialQueues, initialState 1 prgm)
    invertQueues qs = Queues { queueSnd = queueRcv qs, queueRcv = queueSnd qs }
    executePrgm w@(st, qs) = if inside
        then executeInstr2 (program st !! (pc st)) w
        else (st { halt = True }, qs)
        where inside = pc st < length (program st) && pc st >= 0
    execute :: (State, Queues, State) -> (State, Queues, State)
    execute (st1, qs, st2) = if bothHalted || deadlock
        then (st1', qs'', st2')
        else execute (st1', qs'', st2')
        where (st1', qs') = executePrgm (st1, qs)
              rqs' = invertQueues qs'
              (st2', rqs'') = executePrgm (st2, rqs')
              qs'' = invertQueues rqs''
              bothHalted = halt st1' == True && halt st2' == True
              deadlock = wait st1' == True && wait st2' == True

main = do
    input <- getContents
    let prgm = parseProgram input

    print $ part1 prgm
    print $ part2 prgm
