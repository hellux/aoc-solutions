import Data.Map (Map)
import qualified Data.Map as M

type Grid = Map (Int, Int) Node
data Dir = East | North | West | South deriving (Show, Eq, Enum)
data Virus = Virus (Int, Int) Dir deriving (Show)
type World = (Virus, Grid)
data Node = Clean | Weakened | Infected | Flagged deriving (Eq, Enum)

parseGrid rows = M.fromList $ filter ((== Infected) . snd) assocs where
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
back = right . right

queryNode = M.findWithDefault Clean
setNode pos Clean grid = M.delete pos grid
setNode pos node grid = M.insert pos node grid

succNode Flagged = Clean
succNode node = succ node

move (Virus (x, y) dir) = Virus newPos dir
    where newPos = case dir of
                    East  -> (x+1, y)
                    North -> (x, y-1)
                    West  -> (x-1, y)
                    South -> (x, y+1)

countInfections _ 0 _ = 0
countInfections burstR n (v@(Virus pos _), grid) = infection + nextIterations
    where newWorld@(_, newGrid) = burstR (v, grid)
          node = queryNode pos newGrid
          infection = if node == Infected then 1 else 0
          nextIterations = countInfections burstR (n-1) newWorld

burst rules (v@(Virus pos _), grid) = (newVirus, newGrid) where
    node = queryNode pos grid
    (rotation, nextNode) = rules node
    newGrid = setNode pos nextNode grid
    newVirus = move $ rotation v

part1 = countInfections (burst rules) 70 where
    rotation node = if node == Infected then right else left
    newNode  node = if node == Infected then Clean else Infected
    rules node = (rotation node, newNode node)

part2 = countInfections (burst rules) 100 where
    rotation node = case node of
                        Clean -> left
                        Weakened -> id
                        Infected -> right
                        Flagged -> back
    rules node = (rotation node, succNode node)

main = do
    rows <- fmap lines getContents
    let grid = parseGrid rows
    let world = (initialVirus, grid)

    print $ part1 world
    print $ part2 world
