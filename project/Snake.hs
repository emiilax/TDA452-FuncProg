module Snake where
import Data.Maybe
import System.Random
import Test.QuickCheck


-- ------ DATATYPES ------ --
-- Represents the snake-field
data Grid = Grid [[Tile]]
  deriving (Show)

instance Arbitrary Grid where
  arbitrary =
    do k <- genSize
       rows <- sequence [ sequence [ genTile | _ <- [1..k] ] | _ <- [1..k]]
       return (Grid rows)


-- Represents a tile in the grid
data Tile = SnakeBody | Coin | Empty
  deriving (Eq,Show)

-- Represents a position in the grid
type Pos = (Int, Int)


-- Represents a snake. Contains a list of positions
data Snake = Add Pos Snake | End

instance Show Snake where
  show End = "--"
  show (Add pos snake) = show pos ++ show snake

instance Arbitrary Snake where
  arbitrary =
    do (x,y) <- genPos
       return (Add (x+2,y) (Add (x+1,y) (Add (x,y) End)))

instance Arbitrary Tile where
  arbitrary = frequency [ (1, return Coin)
                        ,(40, return Empty)]



-- Represents a collision type.
data CollisionState = CWall | CCoin | CSnake | CNothing
  deriving (Eq, Show)

-- ------ END DATATYPES ------ --


-- ------ GENERATORS ------ --

genSize :: Gen Int
genSize = choose (20,40)

genGrid :: Gen Grid
genGrid = arbitrary

genSnake :: Gen Snake
genSnake = arbitrary

genPos :: Gen Pos
genPos = do x <- choose (0, 16)
            y <- choose (0, 20)
            return (x,y)

genTile :: Gen Tile
genTile = arbitrary

-- ------ END GENERATORS ------ --


-- ------ FUNCTIONS ------ --

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

-- Checks so that a tile gets updated correctly
prop_updateTileInGrid :: Grid -> Pos -> Tile -> Bool
prop_updateTileInGrid (Grid grid) (row, col) tile
  | row >= length grid || col >= length grid || row < 0 || col < 0  = True
  | otherwise = (updatedGrid !! row) !! col == tile

  where (Grid updatedGrid) = updateTileInGrid (Grid grid) (row, col) tile


-- Puts an object on a given place in a list. (Copied from Sudoku. Found it very
-- useful in this project)
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) x (pos,_) | length x < pos+1 = error "(!!=) index bigger than list length"
                | pos < -1        = error "(!!= negative index)"
(!!=) x (pos, a) = take pos x ++ [a] ++ drop (pos+1) x


-- Refreshes the frid with the snakes new positions. It creates a new empty
-- grid and fill it with the Snake, instead of keeping track of snakes old pos
refreshGrid :: Grid -> Snake -> Pos -> Grid
refreshGrid (Grid grid) = refreshGrid' (createGrid (length grid))

refreshGrid' :: Grid -> Snake -> Pos -> Grid
refreshGrid' g End cp = updateTileInGrid g cp Coin
refreshGrid' g (Add pos snake) cp = refreshGrid' updatedGrid snake cp
  where updatedGrid = updateTileInGrid g pos SnakeBody


prop_refreshGrid :: Grid -> Snake -> Pos -> Bool
prop_refreshGrid (Grid grid) snake (row,col)
  | row >= length grid || col >= length grid || row < 0 || col < 0  = True
  | otherwise = checkSnakeInGrid refreshedGrid snake && checkCoinPosInGrid refreshedGrid (row,col)
  where (Grid refreshedGrid) = refreshGrid (Grid grid) snake (row,col)


checkSnakeInGrid :: [[Tile]] -> Snake -> Bool
checkSnakeInGrid _ End = True
checkSnakeInGrid list (Add (row,col) snake) = (list !! row) !! col == SnakeBody
                                                && checkSnakeInGrid list snake


checkCoinPosInGrid :: [[Tile]] -> Pos -> Bool
checkCoinPosInGrid list (row, col) = (list !! row) !! col == Coin

--isConcurrentSnake :: Snake -> Bool
--isConcurrentSnake (Add ())

-- creates an empty grid with a given size.
createGrid :: Int -> Grid
createGrid n = Grid (replicate n (replicate n Empty))


-- Check what kind of collision it is. Used to determine what action that should
-- be made.
collision :: Grid -> Snake -> CollisionState
collision (Grid grid) (Add (row, col) _)          | row > (length grid - 1) || col > (length grid - 1)|| row < 0 || col < 0 = CWall
collision (Grid grid) (Add (row, col) restOfSnake)| isSnakePos (row, col) restOfSnake          = CSnake
                                                  | tile == Coin                               = CCoin
                                                  | otherwise                                  = CNothing
  where tile = (grid !! row) !! col

prop_checkCollision :: Pos -> Grid -> Snake -> Bool
prop_checkCollision (x,y) (Grid grid) (Add headPos rs) | x <= 0 || y <= 0 || x > (length grid) || y > (length grid) = True
                                                       | isSnakePos (x,y) rs  && (x,y) == headPos = collisionState == CSnake
                                                       | (x,y) == headPos = collisionState == CCoin
                                                       | otherwise = collisionState == CNothing
  where updatedGrid = updateTileInGrid (Grid grid) (x,y) Coin
        collisionState = collision updatedGrid (Add headPos rs)

-- Checks wheather a pos is the snakes pos. Used to see if the snake is
-- going into it self.
isSnakePos :: Pos -> Snake -> Bool
isSnakePos _ End = False
isSnakePos (row,col) (Add (sr,sc) rsnake) | row == sr && col == sc = True
                                          | otherwise = isSnakePos (row, col) rsnake


-- Helpfunction to see wheather the body behind the head is behind. (Sorry for vague
-- description). Used in moveSnake to make sure that you dont go the opposite direction
-- since that is not possible
checkSnakePosBehind :: Pos -> Snake -> Bool
checkSnakePosBehind _ End = False
checkSnakePosBehind (row,col) (Add (sr,sc) snake) = row == sr && col == sc

-- Moves the snake in a given direction. Uses checkSnakePosBehind to determine
-- wheather the move is ok or not.
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


-- Returns the tail of the snake.
getSnakeTail :: Snake -> Pos
getSnakeTail (Add pos End ) = pos
getSnakeTail (Add pos snake) = getSnakeTail snake

-- Grows the snake. Puts a snake part at a given position. In the implementation,
-- the position is the snake-tail of snake before it moved.
growSnake :: Snake -> Pos -> Snake
growSnake (Add pos End) tailPos  = Add pos (Add tailPos End )
growSnake (Add pos snake) newPos  = Add pos (growSnake snake newPos)

-- returns a random position on the grid. Used to put the coin on a random place
-- in the grid. We have the snake as input to make sure that the coint not
-- end up on a snake-pos.
ranPos :: StdGen -> Int -> Snake -> Pos
ranPos g n snake | isSnakePos (x,y) snake = ranPos g2 n snake
                 | otherwise = (x,y)
  where (x,g1) = randomR(0, n) g
        (y,g2) = randomR(0, n) g1


-- calculates the score of the snake
score :: Snake -> Int
score snake = snakeLength snake - snakeLength startSnake

-- returns the snake lenght
snakeLength :: Snake -> Int
snakeLength End = 0
snakeLength (Add pos restOfSnake) = 1 + snakeLength restOfSnake



------------ Testing variables ------------

startSnake = (Add (3,6) (Add (3,5) (Add (3,4) (Add (3,3) (Add (3,2) (Add (3,1) End))))))
