import qualified Data.Map as M
import Data.Map (Map)

data Monkey = Yell Int | Wait String String Char

parseMonkeys = M.fromList . map (parseMonkey . words) . filter (not . null) . lines where
    parseMonkey [name, number] = (init name, Yell (read number))
    parseMonkey [name, a, [op], b] = (init name, Wait a b op)

op '+' = (+)
op '-' = (-)
op '*' = (*)
op '/' = div

yell n ms = case ms M.! n of
    Yell i -> i
    Wait a b o -> op o (yell a ms) (yell b ms)

part1 = yell "root"

main = do
    monkeys <- fmap parseMonkeys getContents
    print $ part1 monkeys
