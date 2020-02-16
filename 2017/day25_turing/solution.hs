import Data.List (groupBy)
import qualified Data.Map as M
import Data.Map (Map)

type States = Map StateName State
type StateName = Char
type State = (Action, Action)
data Action = Action { value :: Bool
                     , dir :: Direction
                     , next :: StateName
                     } deriving (Show)
data Direction = L | R deriving (Show)

data Tape = Tape [Bool] [Bool] deriving Show
type Machine = (StateName, Tape)

parseName :: String -> StateName
parseName = head . last . words

parseSteps :: String -> Int
parseSteps = read . (!!5) . words

parseStates :: [String] -> States
parseStates ls = foldl addState M.empty (paragraphs ls) where
    paragraphs = filter ((/=) [""]) . groupBy (\a b -> (a /= "") && (b /= ""))
    addState m (nameLn:ls) = M.insert (parseName nameLn) (parseState ls) m

parseState :: [String] -> State
parseState ls = ( parseAction (take 3 $ drop 1 ls)
                , parseAction (take 3 $ drop 5 ls)
                )

parseAction :: [String] -> Action
parseAction ls =
    Action { value = (==1) $ read $ init $ last $ words $ ls!!0
           , dir = case head $ last $ words (ls!!1) of 'r' -> R; 'l' -> L
           , next = parseName (ls!!2)
           }

parse (startLn:stepsLn:statesLns) = ( parseName startLn
                                    , parseSteps stepsLn
                                    , parseStates statesLns
                                    )

empty = Tape [] []
move L = moveLeft
move R = moveRight
moveRight (Tape feb (a:ft)) = Tape (a:feb) ft
moveRight (Tape feb []) = Tape (False:feb) []
moveLeft (Tape (f:eb) aft) = Tape eb (f:aft)
moveLeft (Tape [] aft) = Tape [] (False:aft)

write a (Tape feb (_:ft)) = Tape feb (a:ft)
write a (Tape feb []) = Tape feb [a]
current (Tape _ (a:_)) = a
current (Tape _ _) = False
checksum (Tape feb aft) = (length (filter id feb))
                        + (length (filter id aft))

tick :: States -> Machine -> Machine
tick states (currentState, tape) = (next action, nextTape)
    where Just state = M.lookup currentState states
          action = if (current tape) then snd state else fst state
          nextTape = move (dir action) $ write (value action) tape

part1 states startState steps = checksum
                              $ snd
                              $ last
                              $ take (steps+1)
                              $ iterate (tick states) (startState, empty)

main = do
    input <- fmap lines getContents
    let (startState, steps, states) = parse input

    print $ part1 states startState steps
