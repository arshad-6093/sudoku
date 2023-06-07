module Lib where

import Data.List (sort)

-- Considering sudokuTable as a tuple of 3 Int. First int denotes the value, and next two tuples as rows and columns.
-- Initialising the sudokuTable with 0 in all rows and columns.

type SudokuTable = [(Int, Int, Int)]

sudokuTable :: SudokuTable
sudokuTable = [(0,x,y) | x <- [1..9], y <- [1..9]]

-- A function index such that index s i j evaluates to the number in the table s : SudokuTable at position (i, j)

index :: SudokuTable -> (Int,Int) -> Int
index [] _ = 0
index ((a,b,c):xs) (i,j)
 | (b,c) == (i,j) = a
 | otherwise = index xs (i,j)

-- A function update such that, given an input table s : SudokuTable, the expression update s i j n evaluates to a
-- table s’ : SudokuTable such that index s’ i j == n, and the rest of s’ is the the same as s. In other words,
--the function updates the cell at position pi, jq with the given value

update :: SudokuTable -> (Int,Int) -> Int -> SudokuTable
update [] _ _ = []
update sTable (i,j) n = map (\x@(_,b,c) -> if (b,c) == (i,j) then (n,i,j) else x ) sTable

-- A function difficulty which, given a table s : SudokuTable, returns the difficulty of a
-- Sudoku grid. The difficulty of a table is defined as the sum of the number of empty cells in
-- the table divided by the total number of cells in the grid.

difficulty :: SudokuTable -> Integer
difficulty [] = 0
difficulty sTable = (sum $ map (\(a,_,_) -> if a == 0 then 1 else 0) sTable) `div` 81

rows :: SudokuTable -> [[Int]]
rows sTable = [[index sTable (b,a) | a <- [1..9]] | b <- [1..9]]

columns :: SudokuTable -> [[Int]]
columns sTable = [[index sTable (a,b) | a <- [1..9]] | b <- [1..9]]

grids :: SudokuTable -> [[Int]]
grids sTable =
  let a = [[1..3],[4..6],[7..9]]
  in reduce $ map (\x -> map (\y -> [index sTable (c,d) | c <- x, d <- y]) a) a

reduce :: [[a]] -> [a]
reduce [] = []
reduce (x:xs) = x ++ reduce xs

-- A function isValid that checks if the given table s : SudokuTable is valid, meaning that
-- each digit from 1 to 9 appears at most once in each row, column, and 3x3 sub-grid (so in
-- particular, a completely empty grid should be valid)

isValid :: SudokuTable -> Bool
isValid sTable = 
  let list = grids sTable ++ columns sTable ++ rows sTable
  in and $ map (\s -> sort s == [1..9] || sort s == [0,0,0,0,0,0,0,0,0]) list

-- A function isComplete that checks if the table is complete, meaning that the table is valid
-- and all cells have been filled with digits.

isComplete :: SudokuTable -> Bool
isComplete sTable =
  let list = grids sTable ++ columns sTable ++ rows sTable
  in and $ map (\s -> sort s == [1..9]) list

-- a pretty-printer that can output a Sudoku table to a file or to a terminal in a human-readable format

sudokuPrinter :: SudokuTable -> IO ()
sudokuPrinter sTable = do
  putStrLn "+-------+-------+-------+"
  _ <- helper (rows sTable !! 0)
  _ <- helper (rows sTable !! 1)
  _ <- helper (rows sTable !! 2)
  putStrLn "+-------+-------+-------+"
  _ <- helper (rows sTable !! 3)
  _ <- helper (rows sTable !! 4)
  _ <- helper (rows sTable !! 5)
  putStrLn "+-------+-------+-------+"
  _ <- helper (rows sTable !! 6)
  _ <- helper (rows sTable !! 7)
  _ <- helper (rows sTable !! 8)
  putStrLn "+-------+-------+-------+"

helper :: [Int] ->IO ()
helper l = do
 putStrLn $ "| " ++ show (l !! 0) ++  " "  ++ show (l !! 1) ++ " " ++ show (l !! 2) ++ " | " ++ show (l !! 3) ++ " "  ++ show (l !! 4)
       ++ " " ++ show (l !! 5) ++ " | " ++ show (l !! 6) ++ " " ++ show (l !! 7) ++  " "  ++ show (l !! 8) ++ " |"

-- a parser that can read in a Sudoku table in a format defined in sample.txt from a file. An incomplete table
-- with unfilled number is represented as 0.

sudokuParser :: IO ()
sudokuParser = do
  f <- readFile "src/sample.txt"
  let x = lines f
      y = filter (\ t -> t /= '|' && t /= ' ') (x !! 1 ++ x !! 2 ++ x !! 3 ++ x !! 5 ++ x !! 6 ++ x !! 7 ++ x !! 9 ++ x !! 10 ++ x !! 11)
      z = [ read ((y !! a ) : "") :: Int  | a <- [0..80]]
      r = reduce [[(w,q) | q <- [1..9]] | w <- [1..9]]
  solving (parsing z r)

parsing :: [Int] -> [(Int,Int)] -> SudokuTable
parsing [] [] = []
parsing [] _ = []
parsing _ [] = []
parsing (x:xs) ((a,b):ys) = (x,a,b) : parsing xs ys

solving :: SudokuTable -> IO ()
solving sTable =
  if isComplete sTable
    then do
      sudokuPrinter sTable
      putStrLn "The grid is Complete. Awesome!!"
    else do
      sudokuPrinter sTable
      putStrLn "The grid is incomplete!"
      putStrLn "do you want to solve manually? type 'Y' for trying manually / type 'N' for solving automatically "
      yesOrNo sTable

yesOrNo :: SudokuTable -> IO ()
yesOrNo sT = do
  yOrN <- getChar
  _ <- getChar
  if yOrN `elem` "YN"
    then
      if yOrN == 'Y'
        then do
          putStrLn "Enter the co-ordinates in the format (i,j) to replace 0 in the table to complete"
          gettingCoord sT
        else sudokuPrinter $ autoSolve sT
    else do
      putStrLn "Invalid! please enter 'Y' or 'N'!"
      yesOrNo sT

gettingCoord :: SudokuTable -> IO ()
gettingCoord sTable = do
  coord <- getLine
  let (i,j) = ((read ((coord !! 1) : "") :: Int) , (read ((coord !! 3) : "") :: Int))
  if index sTable (i,j) == 0
    then 
      if coord !! 0 == '(' && coord !! 4 == ')' && elem (coord !! 1) "123456789" && elem (coord !! 3) "1234566789"
        then do
          putStrLn "Enter the number to fill in the co-ordinate "
          gettingValue (i,j) sTable
        else do
          putStrLn "Format Error! Please enter the co-ordinates in the format (i,j) :: (Int,Int) and i,j less than or equal to 9"
          gettingCoord sTable
    else do
      putStrLn "Cant fill the already filled element"
      gettingCoord sTable

gettingValue :: (Int,Int) -> SudokuTable -> IO ()
gettingValue (a,b) sTable = do
  x <- getChar
  _ <- getLine
  let answer = autoSolve sTable
      value = index answer (a,b)
      v = ((read (x : "")) :: Int)
  if v == value
    then solving (update sTable (a,b) v)
    else do
      putStrLn $ "Impossible to fill the cell " ++ show (a,b) ++ " with " ++ show v
      gettingValue (a,b) sTable

gridHelper :: Int -> Int -> Int
gridHelper a b
  | a <= 3 && b <= 3 = 0
  | a <= 3 && b <= 6 = 1
  | a <= 3 && b <= 9 = 2
  | a <= 6 && b <= 3 = 3
  | a <= 6 && b <= 6 = 4
  | a <= 6 && b <= 9 = 5
  | a <= 9 && b <= 3 = 6
  | a <= 9 && b <= 6 = 7
  | otherwise = 8

uniqueElement :: [Int] -> [Int] -> [Int]
uniqueElement [] _ = []
uniqueElement (x:xs) a =
  if x `elem` a
    then uniqueElement xs a
    else [x] ++ uniqueElement xs a

filtering :: [Int] -> [Int]
filtering [] = []
filtering l =
  if isUnique l
    then l
    else filter (\e -> e >0 ) $ map (\z -> if length z == 3 then head z else 0) [filter (\x -> x == y) l | y <- [1..9]]

isUnique :: [Int] -> Bool
isUnique [] = True
isUnique (x:xs) =
  if x `elem` xs
    then False && isUnique xs
    else True && isUnique xs

autoSolve :: [(Int, Int, Int)] -> [(Int, Int, Int)]
autoSolve sT = do
  if isComplete sT
    then sT
    else
      let x = [((possibleValues (b,c) sT), b,c) | (a,b,c) <- sT, a == 0]
      in autosolveHelper x sT

possibleValues :: (Int,Int) -> SudokuTable -> [Int]
possibleValues (a,b) sTable = do
  let r = uniqueElement [1..9] $ filter (>0) $ (rows sTable) !! (a-1)
      c = uniqueElement [1..9] $ filter (>0) $ (columns sTable) !! (b-1)
      g = uniqueElement [1..9] $ filter (>0) $ (grids sTable) !! (gridHelper a b)
  filtering (r++c++g)

autosolveHelper :: [([Int], Int, Int)] -> SudokuTable -> SudokuTable
autosolveHelper [] a = a
autosolveHelper ((l,a,b):ys) sT =
  if length l == 1
    then do
      let updatedSt = update sT (a,b) (head l)
          updatedx = [((possibleValues (e,f) updatedSt), e,f) | (d,e,f) <- updatedSt, d == 0]
      autosolveHelper updatedx updatedSt
    else autosolveHelper ys sT