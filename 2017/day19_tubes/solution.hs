import Data.Array
import Data.Maybe (fromJust)
import Data.List (findIndex)

data Tile = Letter Char
          | Hori
          | Vert
          | Cross
          | Empty
          deriving (Eq, Show)
type Diagram = Array (Int, Int) Tile
data Direction = R | U | L | D deriving (Show, Enum)

left R = D
left dir = pred dir
right D = R
right dir = succ dir

parseTile '|' = Vert
parseTile '-' = Hori
parseTile '+' = Cross
parseTile ' ' = Empty
parseTile c   = Letter c

parseDiagram input = array ((1, 1), (width, height)) indexedTiles where
    height = length $ lines input
    width = maximum $ map length $ lines input
    pad row = row ++ replicate (width-length row) Empty
    tiles = map (pad . map parseTile) $ lines input
    indexedTiles = [ ((x, y), tile) | (y, row) <- zip [1..] tiles,
                                      (x, tile) <- zip [1..] row ]

move R (x, y) = (x+1, y)
move U (x, y) = (x, y-1)
move L (x, y) = (x-1, y)
move D (x, y) = (x, y+1)

query pos diag = if elem pos (indices diag) then diag ! pos else Empty

startPos diag = (startCol, 1) where
    startCol = ( (+1)
               . fromJust
               . findIndex (==Vert)
               . map snd
               . filter (\((_, y), _) -> y==1)
               . assocs
               ) diag

followPath dir pos diag = case tile of
    Cross    -> let newDir = if query (move (left dir) pos) diag == Empty
                             then right dir else left dir
                in tile : followPath newDir (move newDir pos) diag
    Letter c -> tile : followPath dir (move dir pos) diag
    Empty    -> []
    _        -> tile : followPath dir (move dir pos) diag
    where tile = query pos diag

part1 [] = []
part1 (Letter c:xs) = c : part1 xs
part1 (_:xs) = part1 xs

part2 = length

main = do
    input <- getContents
    let diag = parseDiagram input :: Diagram
    let path = followPath D (startPos diag) diag

    print $ part1 path
    print $ part2 path
