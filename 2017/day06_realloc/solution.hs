range banks = [0..length banks-1]

next i banks = (i+1) `mod` (length banks)

select val banks i
    | banks !! i == val = i
    | otherwise = select val banks (next i banks)

redistribute mem banks i
    | mem <= 0 = banks
    | otherwise = redistribute (mem-1) new_banks (next i banks)
    where new_banks = [if i == j then x+1 else x | j <- range banks,
                       let x = (banks !! j)]

realloc seen cycles i banks
    | elem new_banks seen = new_banks:seen
    | otherwise = realloc (new_banks:seen) (cycles+1) i new_banks
    where new_banks = let selected = (select (maximum banks) banks i)
                      in redistribute (maximum banks)
                        [if j == selected then 0 else banks !! j | j <- range banks]
                        (next selected banks)

part1 = length . (realloc [] 0 0)
part2 = pred . length . (realloc [] 0 0) . head . (realloc [] 0 0)

main :: IO ()
main = do
    input <- getLine
    let banks = map (read::String->Int) $ words input

    print $ part1 banks
    print $ part2 banks
