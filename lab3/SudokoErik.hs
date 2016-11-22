import Data.List
import Data.Maybe
import Data.Char
import Test.QuickCheck
import Prelude

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
  deriving(Show)



allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

allSix :: Sudoku
allSix = Sudoku (replicate 9 (replicate 9 (Just 8)))

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) | length rows == 9 && length (transpose rows) == 9 = True
isSudoku _ = False

isSolved :: Sudoku -> Bool
isSolved (Sudoku a) = isSudoku (Sudoku a) && isRowsSolved a

isRowsSolved :: [[Maybe Int]] -> Bool
isRowsSolved [] = True
isRowsSolved (x:xs) | Nothing `elem` x = False
                    | otherwise = isRowsSolved xs


printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku []) = putStr ""
printSudoku (Sudoku l) | isSudoku (Sudoku l) = putStr(createSudokuString l)
                       | otherwise =  error "printSudoku: Not a Sudoku"

createSudokuString:: [[Maybe Int]] -> String
createSudokuString [] = ""
createSudokuString (x:xs) = rowString x ++ "\n" ++ createSudokuString xs


rowString:: [Maybe Int] -> String
rowString [] = ""
rowString (x:xs) | isNothing x = "." ++ rowString xs
rowString (Just n:xs) = show n ++ rowString xs

readSudoku :: FilePath -> IO Sudoku
readSudoku file = do f <- readFile file
                     let sudoku = Sudoku (map readRow (lines f))
                     return sudoku

readRow :: String -> [Maybe Int]
readRow []     = []
readRow (x:xs) | x == '.' = Nothing: readRow xs
               | isDigit x = Just (digitToInt x):  readRow xs
               | otherwise = error "readSudoku: Bad input"

cell :: Gen (Maybe Int)
cell = frequency [(9, return(Nothing)), (1, elements [Just n | n <- [1..9]])]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (x:xs) | x `elem` xs && x /= Nothing = False
                   | otherwise = isOkayBlock xs

blocks :: Sudoku -> [Block]
blocks (Sudoku list) = list ++
                       transpose list ++
                       get3By3 (take 3 list) ++
                       get3By3 (take 3 (drop 3 list)) ++
                       get3By3 (take 3 (drop 6 list))

get3By3 :: [[Maybe Int]] -> [Block]
get3By3 list = [concat(take 3 tList)] ++
               [concat(take 3 (drop 3 tList))] ++
               [concat(take 3 (drop 6 tList))]
  where tList = transpose list

isOkay :: Sudoku -> Bool
isOkay sudoku = isSudoku sudoku && checkAllBlocks (blocks sudoku)

checkAllBlocks :: [Block] -> Bool
checkAllBlocks [] = True
checkAllBlocks (x:xs) = isOkayBlock x && checkAllBlocks xs

--An example sudoku for testing
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
