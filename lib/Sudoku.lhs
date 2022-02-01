\section{Sudokus}

Sudokus are a well-known constraint satisfaction problem: each square of the $9 \times 9$ grid is constrained by the squares in the same row, the same column, and the same $3 \times 3$ block.
In order to use the AC-3 algorithm on sudokus, the sudoku first needs to be represented as a constraint satisfaction problem (see \cref{sec:CSP}).
In order to do so, the variables, domains and constraints of the problem need to be specified.

\begin{code}
module Sudoku where

import CSP
import AC3
import Data.List
import Data.Char

sudokuVars :: [Variable]
sudokuVars = [0..80]
\end{code}

We have chosen to represent the 81 squares of the grid as numbers between 0 and 80.
\todo[inline]{say something about being in line with the CSP definition}

The domain of each empty square of a sudoku is $\{ 1, \ldots, 9 \}$; the domain of a square filled with some $x$ is $\{x\}$.
Since a \verb|Domain| in our \verb|CSP| definition also consists of the variable's \verb|Int|, the following code also computes the `index' of the square as a number between 0 and 80.

\todo[inline]{say something about the Python code and its formatting: we input the sudoku we want to solve as a string where empty cells are zeroes, a zero means the starting domain can be anything in $\{ 1, \ldots, 9 \}$, if the cell is given its domain has just that element}

\begin{code}
generateSudokuDomains :: [Value] -> [Domain]
generateSudokuDomains [] = []
generateSudokuDomains (x:xs)
    | x == 0    = (80 - length xs, [1..9]):generateSudokuDomains xs
    | otherwise = (80 - length xs, [x]   ):generateSudokuDomains xs
\end{code}

Arguably the most interesting part now is how the constraints for each variable are generated.
The function \verb|generateSudokuConstraints| takes the list of all variables of the sudoku, and returns the list of constraints for the sudoku. It creates this list of constraints by working through the list of variables one by one and generating all constraints for each variable.
As said before, each square on the grid is constrained by its row, column and $3 \times 3$ block.
So a variable $n$ is a member of all arcs $\langle n, x \rangle$ where $x$ is a variable in the same row, column or block.
The allowable values for the pair $\langle n, x \rangle$ are then all $y_1, y_2 \in \{ 1, \ldots, 9 \}$ such that $y_1 \neq y_2$.

\begin{code}
generateSudokuConstraints :: [Variable] -> [Constraint]
generateSudokuConstraints [] = []
generateSudokuConstraints (n:xs) =
  map (\x -> ( (n,x), [(y1,y2) | y1 <- [1..9], y2 <- [1..9], y1 /= y2] ) )
\end{code}

The row, column and block constraints are dependent on the position of the variable $n$ within the grid.

The following code fragment determines the variables $x$ with which $n$ is participating in a constraint.

The variables in $n$'s row are obtained by rounding $n$ down to the nearest multiple of 9 (i.e. taking $n - (n \mod 9)$) and then adding $i \in \{ 0, \ldots 8 \}$ to it.

The variables in $n$'s column are obtained by getting the `$y$-coordinate' of $n$ using $n \mod 9$, and adding multiples of 9 to it.

The code for obtaining the variables in $n$'s block is more complex.

\begin{code}
-- eg if the y position is the middle row of the 3x3 square we have (n div 9) mod 3 == 1, and so we find the other square variables by also looking the row above (j = -1) and below (j = 1)
      -- the variables in its row are found by subtracting until we get a multiple of 9 and by adding until the next one
      -- and the same action for the column are found by taking the y position
  (filter (/=n) (
    nub (
      -- rows
         [n - (n `mod` 9) + i | i <- [0..8]]
      -- columns
      ++ [n `mod` 9 + 9 * j   | j <- [0..8]]
      -- blocks
      ++ [n - (n `mod` 3) + i + 9 * (j - ((n `div` 9) `mod` 3)) | i <- [0..2], j <- [0..2]]
    )
  ))
  ++ generateSudokuConstraints xs
\end{code}

We \verb|filter| the output such that there will not be an arc $ \langle n, n \rangle $ in the constraints, since there will be no assignment that satisfies the constraint $n \neq n$.
Moreover, we use \verb|nub| to ensure that there are no duplicate constraints.
\todo[inline]{mention something about double constraints because (x,y) /= (y, x)}

\begin{code}
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
  sudokuString <- readFile "sudoku/sudoku.txt"
  -- make the string into a list of Ints
  let values = map digitToInt sudokuString
  -- solve the sudoku and print it
  do printSudoku $ ac3domain sudokuVars (generateSudokuDomains values) (generateSudokuConstraints sudokuVars)
\end{code}
