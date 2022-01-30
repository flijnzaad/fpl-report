\section{Sudokus}

\begin{code}
module Sudoku where

import CSP
import AC3
import Data.List
import Data.Char
-- given a variable we create the arcs that correspond to all other variables in the same square, row or column
generateSudokuConstraints :: [Variable] -> [Constraint]
generateSudokuConstraints [] = []
-- given an arc, all pairs with type (Value, Value) with different digits are allowed
generateSudokuConstraints (n:xs) = map (\x -> ((n,x), [(y1,y2) | y1 <-[1..9], y2 <- [1..9], y1 /= y2]))
-- the other variables in the square are found by finding the x-axis and y-axis position of the current variable in its square
    -- eg if the y position is the middle row of the 3x3 square we have (n div 9) mod 3 == 1, and so we find the other square variables by also looking the row above (j = -1) and below (j = 1)
    (filter (/=n) (nub ([n + i + 9*j | i <- [- (n `mod` 3) .. 2- (n `mod` 3)], j <- [- (n `div` 9 `mod` 3) .. 2- (n `div` 9 `mod` 3)]] ++
        -- the variables in its row are found by subtracting until we get a multiple of 9 and by adding until the next one
        [n + i | i <- [- (n `mod` 9) .. 8 - n `mod` 9]] ++
        -- and the same action for the column are found by taking the y position
        [n + 9*i | i <- [- (n `div` 9 `mod` 9) .. 8- (n `div` 9 `mod` 9)]])))
        ++ generateSudokuConstraints xs

-- we input the sudoku we want to solve as a string where empty cells are zeroes
generateSudokuDomains :: [Value] -> [Domain]
generateSudokuDomains [] = []
-- a zero means the starting domain can be any thin in [1..9], if the cell is given its domain has just that element
generateSudokuDomains (x:xs) | x == 0    = (80 - length xs, [1..9]):generateSudokuDomains xs
                             | otherwise = (80 - length xs, [x]):generateSudokuDomains xs

sudokuVars :: [Variable]
sudokuVars = [0..80]

-- test: ac3 (CSP sudokuVars (generateSudokuDomains sudoku1) (generateSudokuConstraints sudokuVars), True, generateSudokuConstraints sudokuVars)
-- test: ac3domain sudokuVars (generateSudokuDomains sudoku1) (generateSudokuConstraints sudokuVars)

-- prints a sudoku
printSudoku :: [Domain] -> IO ()
-- base case recursion: done printing
printSudoku [] = putStr ""
printSudoku ((n, val@(value:_)):xs) =
  do
    putStr (if val == [value] then show value else "_")
    if n `mod` 3 == 2
       -- put spaces between different blocks
      then putStr " "
      else putStr ""
    if n `mod` 9 == 8
       -- put newlines at the end of rows
      then putStr "\n"
      else putStr ""
    if n `mod` 27 == 26
       -- put extra newlines to vertically separate blocks
      then putStr "\n"
      else putStr ""
    do printSudoku xs
-- (to avoid warning about non-exhaustive cases)
printSudoku _ = putStr ""

-- solves sudoku in "sudoku.txt" in current directory
solveSudokuFromFile :: IO ()
solveSudokuFromFile = do
  sudokuString <- readFile "./sudoku.txt"
  -- make the string into a list of Ints
  let values = map digitToInt sudokuString
  -- solve the sudoku and print it
  do printSudoku $ ac3domain sudokuVars (generateSudokuDomains values) (generateSudokuConstraints sudokuVars)
\end{code}
