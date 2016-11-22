import Data.Maybe
import Data.List

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
  deriving(Show,Eq)


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

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (take 9(repeat(take 9(repeat(Nothing)))))

allSix :: Sudoku
allSix = Sudoku (take 9(repeat(take 9(repeat(Just 6)))))

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) = (length rows == 9) && (length (transpose rows)) == 9

isSolved :: Sudoku -> Bool
isSolved (Sudoku rows) | isSudoku (Sudoku rows) = isSolvedHelp rows

isSolvedHelp :: [[Maybe Int]] -> Bool
isSolvedHelp [] = True
isSolvedHelp (x:xs) | Nothing `elem` x = False
                    | otherwise = isSolvedHelp xs
