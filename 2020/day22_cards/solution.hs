type Deck = [Int]
type State = (Deck, Deck)

playRound :: State -> State
playRound (p1:p1s, p2:p2s)
    | p1 > p2   = (p1s ++ [p1, p2], p2s)
    | otherwise = (p1s, p2s ++ [p2, p1])

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse

playGame :: State -> Int
playGame (p1, []) = score p1
playGame ([], p2) = score p2
playGame state = playGame $ playRound state

parseInput :: String -> State
parseInput input = (map read (drop 1 p1), map read (drop 2 p2)) where
    l = lines input
    (p1, p2) = splitAt (div (length l) 2) l

part1 = playGame

main = do
    input <- getContents
    let initial = parseInput input

    print $ part1 initial
