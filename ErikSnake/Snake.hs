module Snake where
import Data.Maybe
import System.Random

-- Represents the snake-field
data Grid = Grid [[Tile]]

-- Represents a tile in the grid
data Tile = SnakeBody | Coin | Empty
  deriving (Eq)
-- Represents a position in the grid
type Pos = (Int, Int)

-- Represents a snake. Contains a list of positions
data Snake = Add Pos Snake | End
instance Show Snake where
  show End = " "
  show (Add pos snake) = show pos ++ show snake

-- Represents a collision type.
data CollisionState = CWall | CCoin | CSnake | CNothing
  deriving (Eq, Show)

-- Returns the grid as a String
showGrid :: Grid -> String
showGrid (Grid grid) = unlines $ map showRow grid
  where showRow = map showPos
        showPos Empty = '-'
        showPos SnakeBody= '*'
        showPos Coin = 'o'

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
refreshGrid :: Grid -> Snake -> Pos -> Grid
refreshGrid (Grid grid) snake cp = refreshGrid' (createGrid (length grid)) snake cp

refreshGrid' :: Grid -> Snake -> Pos -> Grid
refreshGrid' g End cp = updateTileInGrid g cp Coin
refreshGrid' g (Add pos snake) cp = refreshGrid' updatedGrid snake cp
  where updatedGrid = updateTileInGrid g pos SnakeBody

-- creates an empty grid
createGrid :: Int -> Grid
createGrid n = Grid (replicate n (replicate n Empty))



collision :: Grid -> Snake -> CollisionState
collision _ (Add (row, col) restOfSnake)          | row > 14 || col > 14 || row < 0 || col < 0 = CWall
collision (Grid grid) (Add (row, col) restOfSnake)| isSnakePos (row, col) restOfSnake          = CSnake
                                                  | tile == Coin                               = CCoin
                                                  | otherwise                                  = CNothing
  where tile = (grid !! row) !! col


isSnakePos :: Pos -> Snake -> Bool
isSnakePos _ End = False
isSnakePos (row,col) (Add (sr,sc) rsnake) | row == sr && col == sc = True
                                          | otherwise = isSnakePos (row, col) rsnake


checkSnakePosBehind :: Pos -> Snake -> Bool
checkSnakePosBehind _ End = False
checkSnakePosBehind (row,col) (Add (sr,sc) snake) = row == sr && col == sc
-- Moves the snake in a given direction
moveSnake :: Snake -> String -> Snake
moveSnake (Add (row,col) snake) dir | dir == "down"  && not(checkSnakePosBehind (row+1,col) snake)= Add (row+1,col) restOfSnake
                                    | dir == "down"  = Add (row-1,col) restOfSnake
                                    | dir == "up"    && not(checkSnakePosBehind (row-1,col) snake)= Add (row-1,col) restOfSnake
                                    | dir == "up"    = Add (row+1,col) restOfSnake
                                    | dir == "left"  && not(checkSnakePosBehind (row,col-1) snake) = Add (row,col-1) restOfSnake
                                    | dir == "left"  = Add (row,col+1) restOfSnake
                                    | dir == "right" && not(checkSnakePosBehind (row,col+1) snake)= Add (row,col+1) restOfSnake
                                    | dir == "right" = Add (row,col-1) restOfSnake
                                    | otherwise = error "moveSnake: Direction not allowed"
    where
      restOfSnake = moveSnake' (row,col) snake
      moveSnake' _ End = End
      moveSnake' pos (Add pos1 snake) = Add pos (moveSnake' pos1 snake)


{-growSnake :: Snake -> Snake
growSnake (Add (x,y) End)   = (Add (x,y) (Add (x+1,y) End))
growSnake (Add pos restSnake) = (Add pos (growSnake restSnake))
-}
growSnake :: Snake -> Pos -> Snake
growSnake snake newPos = (Add newPos snake)

ranPos :: StdGen -> Int -> Pos
ranPos g n = (x,y)
  where (x,g1) = randomR(0, n) g
        (y,g2) = randomR(0, n) g1


------------ Testing variables ------------

snake = (Add (3,6) (Add (3,5) (Add (3,4) (Add (3,3) (Add (3,2) (Add (3,1) End))))))

shortsnake:: Snake
shortsnake = Add (4,4) End

s1 = moveSnake snake "right"
g1 = refreshGrid grid s1

s2 = moveSnake s1 "up"
g2 = refreshGrid grid s2


grid = createGrid 15
