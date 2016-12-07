import Data.List
import Data.List.Split
import Data.Maybe
import Prelude
import Data.Char
import Test.QuickCheck

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
  deriving(Show,Eq)


-- creates a blank suduko. firsts creates a list with 9 "Nothing", and then
-- a list with 9 lists containing "Nothing", which results in a 9x9 blank suduko
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- Checks wheather a sudoku is of correct size. first check if there are 9
-- rows, and then check so that all rows contains 9 elements (9 columns) with
-- isSudoku columns
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku m) = length m == 9 && all isRow m
  where
    isRow xs = length xs == 9 && all inRange (catMaybes xs)

-- Checks so that a given value is in the range that fits in a sudou (1..9)
inRange :: Int -> Bool
inRange n = n > 0 && n < 10


-- checks weather the sudoku is "solved" or not. a sudoku is solved if its a
-- sudoku and no cell contains "Nothing" (uses isRowsSolved to determine if the
-- sudoku contains "Nothing").
isSolved :: Sudoku -> Bool
isSolved (Sudoku m) = all isJust $ concat m


-- method used to print a sudoku. uses a helpfunction rowString to create
-- strings of each row. the rows are then concatinaded and then prints.
printSudoku :: Sudoku -> IO ()
printSudoku sud = putStrLn (showSudoku sud)

-- creates a sudoku string from a sudoku
showSudoku :: Sudoku -> String
showSudoku (Sudoku l) = unlines $ map showRow l
  where showRow = map showCell
        showCell Nothing = '.'
        showCell (Just n) = intToDigit n


-- reads suduko from a file. uses a helpfunction readSudoku'
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do file <- readFile f
                  let sudoku = readSudoku' file
                  return sudoku

-- used in readSudoku. Converts every row in the given String into a
-- sudoku list and returns the Sudoku
readSudoku' :: String -> Sudoku
readSudoku' m = Sudoku (map readRow $ lines m)
     where
       readRow = map readCell
       readCell '.' = Nothing
       readCell c   = Just (digitToInt c)

-- creates a arbitrary cell. frequency is used so that we can determine the
-- probability for a cell with "Nothing" or a digit. we uses 9 on "Nothing" and 1
-- on "Just n" since the assignment said 90% vs 10%
cell :: Gen (Maybe Int)
cell = frequency [(9, return Nothing ),
                  (1, fmap Just $ choose (1,9))]

-- Given in the assignment. An abitrary Sudoku
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- checks so that the sudokus generated by the Arbitary Sudoku is correct
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku


type Block = [Maybe Int]

-- checks so that a block don't contains more than one of the same value
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (x:xs) | x `elem` xs && x /= Nothing = False
                   | otherwise = isOkayBlock xs

-- creates a list of all blocks in the suduko. first all the rows, then all the
-- columns (transpose the list) and then all the 3x3 blocks (uses helpfuntion
-- get3x3Block)
blocks :: Sudoku -> [Block]
blocks (Sudoku l) = l ++ transpose l ++ get3x3Block l

-- returns all 3x3 blocks in the sudoku. It first gets the 3x3 boxes generated
-- by the first 3 rows. Then it moves to the next three rows, and finaly the last
-- 3 rows
--       1     |     2     |     3           <-- the splitup
-- -------------------------------------
-- | * | * | * | * | * | * | * | * | * |     <-- x
-- -------------------------------------
-- | * | * | * | * | * | * | * | * | * |     <-- y
-- -------------------------------------
-- | * | * | * | * | * | * | * | * | * |     <-- z
-- -------------------------------------
-- ..          |           |
-- ..
get3x3Block :: [[Maybe Int]] -> [Block]
get3x3Block [] = []
get3x3Block (x:y:z:xs) = (x1 ++ y1 ++ z1): (x2 ++ y2 ++ z2) : (x3 ++ y3 ++ z3) : get3x3Block xs
  where [x1,x2,x3] = chunksOf 3 x
        [y1,y2,y3] = chunksOf 3 y
        [z1,z2,z3] = chunksOf 3 z


-- function checks so that the sudoku isSudoku and so that non of the blocks
-- contains more than one of each element
isOkay :: Sudoku -> Bool
isOkay sud = all isOkayBlock $ blocks sud
