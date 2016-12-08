module Snake where

import Data.Maybe

-- Represents the snake-field
data Grid = Grid [[Tile]]

-- Represents a tile in the grid
data Tile = Filled | Empty

-- Represents a position in the grid
type Pos = (Int, Int)

-- Represents a snake. Contains a list of positions
data Snake = Add Pos Snake | End
instance Show Snake where
  show End = " "
  show (Add pos snake) = show pos ++ show snake

-- Returns the grid as a String
showGrid :: Grid -> String
showGrid (Grid grid) = unlines $ map showRow grid
  where showRow = map showPos
        showPos Empty = '-'
        showPos Filled= '*'

-- prints the grid
printGrid :: Grid -> IO ()
printGrid g = putStr (showGrid g)

-- updates a tile in the grid
updateTileInGrid :: Grid -> Pos -> Tile -> Grid
updateTileInGrid (Grid grid) (row, col) tile = newGrid
  where newGrid = Grid (grid !!= (row,((grid !! row) !!= (col,tile))))



-- copied from Sudoku. Found it very useful in this
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) x (pos,_) | length x < pos = error "(!!=) index bigger than list length"
                | pos < 0        = error "(!!= negative index)"
(!!=) x (pos, a) = take pos x ++ [a] ++ drop (pos+1) x


-- Refreshes the frid with the snakes new positions. It creates a new empty
-- grid and fill it with the Snake, instead of keeping track of snakes old pos
refreshGrid :: Grid -> Snake -> Grid
refreshGrid (Grid grid) snake = refreshGrid' (createGrid (length grid)) snake
  where
    refreshGrid' g End = g
    refreshGrid' g (Add pos snake) = refreshGrid' updatedGrid snake
      where updatedGrid = updateTileInGrid g pos Filled

-- creates an empty grid
createGrid :: Int -> Grid
createGrid n = Grid (replicate n (replicate n Empty))

-- Moves the snake in a given direction
moveSnake :: Snake -> String -> Snake
moveSnake (Add (row,col) snake) dir | dir == "up"    = Add (row+1,col) restOfSnake
                                    | dir == "down"  = Add (row-1,col) restOfSnake
                                    | dir == "left"  = Add (row,col-1) restOfSnake
                                    | dir == "right" = Add (row,col+1) restOfSnake
  where
    restOfSnake = moveSnake' (row,col) snake
    moveSnake' _ End = End
    moveSnake' pos (Add pos1 snake) = Add pos (moveSnake' pos1 snake)


------------ Testing variables ------------

snake = (Add (1,3) (Add (1,2) (Add (1,1) End)))

shortsnake:: Snake
shortsnake = Add (4,4) End

s1 = moveSnake snake "right"
g1 = refreshGrid grid s1

s2 = moveSnake s1 "up"
g2 = refreshGrid grid s2


grid = createGrid 5


testGrid :: Grid
testGrid = Grid [[Empty, Empty, Empty],
                 [Empty, Empty, Empty],
                 [Filled, Filled, Filled]]
