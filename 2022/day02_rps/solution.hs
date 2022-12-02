data Hand = R | P | S deriving (Enum, Eq)

hand "A" = R
hand "B" = P
hand "C" = S
hand "X" = R
hand "Y" = P
hand "Z" = S

shape_score x = fromEnum x + 1

winner_against R = P
winner_against P = S
winner_against S = R

outcome_score o p
    | o == p = 3
    | winner_against o == p = 6
    | otherwise = 0

round_score o p = shape_score p + outcome_score o p

part1 = sum . map row where row [o, p] = round_score (hand o) (hand p)

loser_against R = S
loser_against P = R
loser_against S = P

play o "X" = loser_against o
play o "Y" = o
play o "Z" = winner_against o

part2 = sum . map row where row [o, p] = round_score (hand o) (play (hand o) p)

main = do
    input <- fmap (map words . lines) getContents
    print $ part1 input
    print $ part2 input
