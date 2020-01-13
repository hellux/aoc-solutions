import Data.List (sortBy, groupBy, minimumBy)
import Data.Function (on)

data Particle = Particle { num :: Int
                         , pos :: [Int]
                         , vel :: [Int]
                         , acc :: [Int]
                         } deriving (Eq, Show)

parseParticles = map parseParticle . zip [0..] . lines
parseParticle (i, p) = Particle { num = i
                                , pos = ps!!0
                                , vel = ps!!1
                                , acc = ps!!2
                                } where
    readList xs = read $ "[" ++ xs ++ "]" :: [Int]
    strip = filter (`elem` "-,0123456789")
    rmTrailingComma xs
        | xs == [] = xs
        | last xs == ',' = init xs
        | otherwise = xs
    ps = ( map readList
         . filter (/=[])
         . map (rmTrailingComma . strip)
         . words
         ) p

tick (Particle num pos vel acc) = Particle num newPos newVel acc where
    newVel = zipWith (+) vel acc
    newPos = zipWith (+) pos newVel
tickAll = collide . map tick

manhattan = sum . map abs
multiCmp = foldl1 mappend . map (compare `on`)

-- detect collisions and remove any colliding particles
collide ps = filter (not . (`elem` colliding)) ps where
    colliding = ( concat
                . filter ((>1) . length)
                . groupBy ((==) `on` pos)
                . sortBy (compare `on` pos)
                ) ps

part1 = num . minimumBy (multiCmp $ map (manhattan .) [acc, vel, pos])
part2 = length . last . take 100 . iterate tickAll

main = do
    input <- getContents
    let particles = parseParticles input

    print $ part1 particles
    print $ part2 particles
