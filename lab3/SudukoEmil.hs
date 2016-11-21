import Data.List
import Data.Maybe
import Prelude

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
  deriving(Show)



allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

allSix :: Sudoku
allSix = Sudoku (replicate 9 (replicate 9 (Just 8)))

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) | length rows == 9 = isSudokuRows rows
isSudoku _ = False

isSudokuRows :: [[Maybe Int]] -> Bool
isSudokuRows [] = True
isSudokuRows (x:xs) | length x == 9 = isSudokuRows xs
isSudokuRows _ = False


isSolved :: Sudoku -> Bool
isSolved (Sudoku a) = isSudoku (Sudoku a) && isRowsSolved a

isRowsSolved :: [[Maybe Int]] -> Bool
isRowsSolved [] = True
isRowsSolved (x:xs) | Nothing `elem` x = False
                    | otherwise = isRowsSolved xs


printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku []) = putStr ""
printSudoku (Sudoku l) = putStr(createSudokuString l)

createSudokuString:: [[Maybe Int]] -> String
createSudokuString [] = ""
createSudokuString (x:xs) = rowString x ++ "\n" ++ createSudokuString xs


rowString:: [Maybe Int] -> String
rowString [] = ""
rowString (x:xs) | isNothing x = "." ++ rowString xs
rowString (Just n:xs) = show n ++ rowString xs


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
