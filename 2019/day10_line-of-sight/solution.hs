import Data.List (sort, sortBy, nub)
import Data.Array.IArray
import Data.Maybe (fromJust, listToMaybe, catMaybes)
import Data.Fixed (mod')
import Data.Function (on)
import Control.Monad.State

type Pos = (Int, Int)
type Dir = (Int, Int)
data Square = Empty | Asteroid deriving (Show, Eq)
type Belt = Array (Int, Int) Square

parseSquare '.' = Empty
parseSquare '#' = Asteroid
parse :: String -> Belt
parse input = array ((0, 0), (w-1, h-1)) associations
    where lists = map (map parseSquare) $ lines input
          (w, h) = (length $ head lists, length lists)
          associations = [ ((x, y), square) | (y, row)    <- zip [0..] lists,
                                              (x, square) <- zip [0..] row ]

dims belt = let (_, (x, y)) = bounds belt in (x+1, y+1)

directions :: Belt -> [Dir]
directions belt = full where
    width = let (w, h) = dims belt in max w h
    quad = [ (x, y) | x <- [0..width-1], y <- [1..width-1] ]
    unit (x, y) = let d = gcd x y in (div x d, div y d)
    uniques = nub $ sort $ map unit quad
    orientations (x, y) = [ (x, y), (-y, x), (-x, -y), (y, -x)]
    full = concatMap orientations uniques

move (dx, dy) (x, y) = (x+dx, y+dy)

closest :: Belt -> Pos -> Dir -> Maybe Pos
closest belt pos dir = listToMaybe
                     $ filter onAsteroid
                     $ takeWhile inside
                     $ tail
                     $ iterate (move dir) pos where
    (w, h) = dims belt
    inside (x, y) = 0 <= x && x < w
                 && 0 <= y && y < h
    onAsteroid = (== Asteroid) . (belt !)

visible :: Belt -> Pos -> [Pos]
visible belt pos = catMaybes $ map (closest belt pos) (directions belt)

optimalPos belt = last
                $ sortBy (compare `on` (length . visible belt))
                $ asteroids
    where asteroids = map fst
                    $ filter (\(_, sq) -> sq == Asteroid)
                    $ assocs
                    $ belt

part1 belt = length $ visible belt $ optimalPos belt

angle (x, y) = let (x', y') = (fromIntegral x, fromIntegral y)
               in negate $ atan2 x' y'


vaporize :: Pos -> State (Belt, [Dir]) Pos
vaporize pos = do
    (belt, dirs) <- get

    let (hit:nextDirs) = dropWhile ((== Nothing) . closest belt pos) dirs
    let target = fromJust $ closest belt pos hit
    let nextBelt = belt // [(target, Empty)]

    put (nextBelt, nextDirs)
    return target

part2 belt = 100*x + y
    where pos = optimalPos belt
          dirs = cycle $ sortBy (compare `on` angle) $ directions belt
          (x, y) = last $ evalState (replicateM 200 (vaporize pos)) (belt, dirs)

main = do
    input <- getContents
    let belt = parse input

    print $ part1 belt
    print $ part2 belt
