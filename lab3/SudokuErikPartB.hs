import Data.List
import Data.List.Split
import Data.Maybe
import Prelude
import Data.Char
import Test.QuickCheck

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
  deriving(Show)


-- creates a blank suduko. firsts creates a list with 9 "Nothing", and then
-- a list with 9 lists containing "Nothing", which results in a 9x9 blank suduko
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- Checks wheather a sudoku is of correct size. first check if there are 9
-- rows, and then check so that all rows contains 9 elements (9 columns) with
-- isSudoku columns
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) = length rows == 9 && isSudokuColumns rows

-- has a suduko table ([[Maybe Int]]) as input, and it cheks so that all
-- rows has the lenght 9
isSudokuColumns :: [[Maybe Int]] -> Bool
isSudokuColumns [] = True
isSudokuColumns (x:xs) = length x == 9 && isCorrectValue x && isSudokuColumns xs

--checks that every value in a row is between 1 - 9 or nothing
isCorrectValue :: [Maybe Int] -> Bool
isCorrectValue [] = True
isCorrectValue (Nothing:xs) = isCorrectValue xs
isCorrectValue ((Just x):xs) | x > 0 && x < 10 = isCorrectValue xs
                             | otherwise = False

-- checks weather the sudoku is "solved" or not. a sudoku is solved if its a
-- sudoku and no cell contains "Nothing" (uses isRowsSolved to determine if the
-- sudoku contains "Nothing").
isSolved :: Sudoku -> Bool
isSolved (Sudoku a) = isSudoku (Sudoku a) && isRowsSolved a

-- checks wheather the board contains the element "Nothing" or not. Returns
-- true if not contains, otherwise false
isRowsSolved :: [[Maybe Int]] -> Bool
isRowsSolved [] = True
isRowsSolved (x:xs) | Nothing `elem` x = False
                    | otherwise = isRowsSolved xs

-- method used to print a sudoku. uses a helpfunction rowString to create
-- strings of each row. the rows are then concatinaded and then prints.
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku l) = do let s = concatMap rowString l
                            putStrLn s

-- creates a string that represents a row.
rowString:: [Maybe Int] -> String
rowString [] = "\n"
rowString (Nothing:xs) = "." ++ rowString xs
rowString (Just n:xs) = show n ++ rowString xs

-- reads suduko from a file. uses a helpfunction readRow to create a
-- [Maybe Int] from a row in the file.
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do file <- readFile f
                  let sudoku = Sudoku (map readRow (lines file))
                  return sudoku

-- used in readSudoku. used to read a row and return it as a [Maybe Int]
readRow::String -> [Maybe Int]
readRow [] = []
readRow (x:xs) | isDigit x = Just (digitToInt x) : readRow xs
               | otherwise = Nothing : readRow xs

-- creates a arbitrary cell. frequency is used so that we can determine the
-- probability for a cell with "Nothing" or a digit. we uses 9 on "Nothing" and 1
-- on "Just n" since the assignment said 90% vs 10%
cell :: Gen (Maybe Int)
cell = frequency [(9, return Nothing ),
                  (1, elements [Just n | n <- [1..9]])]

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
isOkay sud = isSudoku sud && allBlocksOk (blocks sud)

-- method used to check so that non of the blocks in the list contains more than
-- one of each elemet
allBlocksOk :: [Block] -> Bool
allBlocksOk [] = True
allBlocksOk (x:xs) = isOkayBlock x && allBlocksOk xs




type Pos = (Int,Int)

blanks :: Sudoku -> [Pos]
blanks (Sudoku sud) = getBlankRow 0 sud

getBlankRow :: Int -> [[Maybe Int]] -> [Pos]
getBlankRow _ [] = []
getBlankRow r (x:xs) = getBlankCell r 0 x ++ getBlankRow(r+1) xs

getBlankCell :: Int -> Int -> [Maybe Int] -> [Pos]
getBlankCell _ _ [] = []
getBlankCell r c (Nothing:xs) = (r,c) : getBlankCell r (c+1) xs
getBlankCell r c (x:xs) = getBlankCell r (c+1) xs

isBlank :: Sudoku -> [Pos] -> Bool
isBlank (Sudoku l) pos = posIsBlank l pos

posIsBlank :: [[Maybe Int]] -> [Pos] -> Bool
posIsBlank _ [] = True
posIsBlank l (x:xs) = ((l !! row) !! column) == Nothing && posIsBlank l xs
  where (row,column) = x

prop_blank :: Sudoku -> Bool
prop_blank sud = isBlank sud (blanks sud)

(!!=) :: [a] -> (Int, a) -> [a]
(!!=) x (pos,_) | length x < pos = error "(!!=) index bigger than list length"
                | pos < 0        = error "(!!= negative index)"
(!!=) x (pos, a) = take pos x ++ [a] ++ drop (pos+1) x

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku sudoku) (row,col) value = Sudoku (sudoku !!= (row,((sudoku !! row) !!= (col,value))))

candidates :: Sudoku -> Pos -> [Int]
candidates sudoku pos = candidates' [1..9] sudoku pos

candidates' :: [Int] -> Sudoku -> Pos -> [Int]
candidates' [] _ _            = []
candidates' (x:xs) sudoku pos | isOkay(update sudoku pos (Just x)) = x : candidates' xs sudoku pos
                              | otherwise = candidates' xs sudoku pos

example :: Sudoku
example =
  Sudoku
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

testl :: [Maybe Int]
testl = [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]


test::[[Maybe Int]]
test = [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
       , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
       , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
       , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
       , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
       , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
       , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
       , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
       , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
       ]
