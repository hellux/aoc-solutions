import Data.Map (Map)
import qualified Data.Map as M

type Grid = Map (Int, Int) Node
data Dir = East | North | West | South deriving (Show, Eq, Enum)
data Virus = Virus (Int, Int) Dir deriving (Show)
type World = (Virus, Grid)
data Node = Clean | Infected deriving (Eq)

parseGrid rows = M.fromList $ filter ((==Infected) . snd) assocs where
    parseCell '.' = Clean
    parseCell '#' = Infected
    nested = map (map parseCell) rows
    startX = negate $ length (rows!!0) `div` 2
    startY = negate $ length rows `div` 2
    assocs = [ ((x, y), col) | (y, row) <- zip [startY..] nested,
                               (x, col) <- zip [startX..] row ]

initialVirus = Virus (0, 0) North

left  (Virus pos dir) = Virus pos (if dir == South then East  else succ dir)
right (Virus pos dir) = Virus pos (if dir == East  then South else pred dir)

queryNode = M.findWithDefault Clean
setNode = M.insert

move (Virus (x, y) dir) = Virus newPos dir
    where newPos = case dir of
                    East  -> (x+1, y)
                    North -> (x, y-1)
                    West  -> (x-1, y)
                    South -> (x, y+1)

burst (v@(Virus pos _), grid) = (newVirus, newGrid) where
    infected = queryNode pos grid == Infected
    rotation = if infected then right else left
    newNode = if infected then Clean else Infected
    newGrid = setNode pos newNode grid
    newVirus = move $ rotation v

countInfections 0 _ = 0
countInfections n (v@(Virus pos _), grid) =
    infection + countInfections (n-1) newWorld where
    newWorld@(_, newGrid) = burst (v, grid)
    node = queryNode pos newGrid
    infection = if node == Infected then 1 else 0

part1 = countInfections 10000

main = do
    rows <- fmap lines getContents
    let grid = parseGrid rows
    let world = (initialVirus, grid)

    print $ part1 world
