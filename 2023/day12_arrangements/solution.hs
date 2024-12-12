import Data.List.Split (splitOn)
import Data.List (group)

parseInput :: String -> [(String, [Int])]
parseInput = map ((\[a, b] -> (a, map read $ splitOn "," b)) . words) . lines

potential :: String -> [String]
potential ['?'] = [".", "#"]
potential [c] = [[c]]
potential ('?':t) = map ('.':) pt ++ map ('#':) pt where pt = potential t
potential s@(c:t) = map (c:) $ potential t

desc = map length . filter ((== '#'). head) . group

--arrangements :: String
arrangements s d = length $ filter (== d) $ map desc $ potential s

part1 = sum . map (uncurry arrangements)

main = do
    input <- fmap parseInput getContents
    print $ part1 input