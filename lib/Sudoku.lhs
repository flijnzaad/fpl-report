\section{Sudokus}

Sudokus are a well-known constraint satisfaction problem: each square of the $9 \times 9$ grid is constrained by the squares in the same row, the same column, and the same $3 \times 3$ block.
In order to use the AC-3 algorithm on sudokus, the sudoku first needs to be represented as a constraint satisfaction problem (see \cref{sec:CSP}).
In order to do so, the variables, domains and constraints of the problem need to be specified.

\begin{code}
module Sudoku where

import CSP
import AC3
import Data.Char     -- for using "digitToInt"
import Data.Maybe    -- for using "fromJust"
import Control.Monad -- for using "when"
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
    | x == 0    = (Var (80 - length xs), (map Val [1..9])) : generateSudokuDomains xs
    | otherwise = (Var (80 - length xs), [x])              : generateSudokuDomains xs
\end{code}

Arguably the most interesting part now is how the constraints for each variable are generated.
To be able to formulate the constraints in an intuitive way, the function \verb|varToCoords| takes a variable and returns a tuple of its $x$- and $y$-coordinates within the $9 \times 9$ grid.
\verb|varToCoords| functions as a wrapper around the \verb|varGrid| to eliminate some duplicate code.

\begin{code}
varGrid :: [(Variable, (Int, Int))]
varGrid = zip (map Var [0..80]) [ (i,j) | i <- [0..8], j <- [0..8] ]
varToCoords :: Variable -> (Int, Int)
varToCoords n = fromJust $ lookup n varGrid
\end{code}

Now, the function \verb|generateSudokuConstraints| takes the list of all variables of the sudoku, and returns the list of constraints for the sudoku. It creates this list of constraints by working through the list of variables one by one and generating all constraints for each variable.
As said before, each square on the grid is constrained by its row, column and $3 \times 3$ block.
So a variable $n$ is a member of all arcs $\langle n, x \rangle$ where $x$ is a variable in the same row, column or block.
The allowable values for the pair $\langle n, x \rangle$ are then all $y_1, y_2 \in \{ 1, \ldots, 9 \}$ such that $y_1 \neq y_2$.

\begin{code}
generateSudokuConstraints :: [Variable] -> [Constraint]
generateSudokuConstraints [] = []
generateSudokuConstraints (n:xs) =
  map (\x -> ( (n,x), [(y1,y2) | y1 <- (map Val [1..9]), y2 <- (map Val [1..9]), y1 /= y2] ) )
\end{code}

The row, column and block constraints are dependent on the position of the variable $n$ within the grid.
The following code fragment determines the variables $x$ with which $n$ is participating in a constraint.
The variables in the same row as $n$ have the same $x$-coordinate, and the variables in the same column as $n$ have the same $y$-coordinate.
To obtain the variables in the same $3 \times 3$ block as $n$, we check if the $x$-coordinates of $n$ and $m$ are the same when divided by 3; we do the same for the $y$-coordinates.

\begin{code}
    (
      -- rows
         [m | m <- (map Var [0..80]), m /= n,
              fst (varToCoords m) == fst (varToCoords n)]
      -- columns
      ++ [m | m <- (map Var [0..80]), m /= n,
              snd (varToCoords m) == snd (varToCoords n)]
      -- blocks
      ++ [m | m <- (map Var [0..80]), m /= n,
              fst (varToCoords m) /= fst (varToCoords n),
              snd (varToCoords m) /= fst (varToCoords n),
              fst (varToCoords m) `div` 3 == fst (varToCoords n) `div` 3,
              snd (varToCoords m) `div` 3 == snd (varToCoords n) `div` 3]
    )
      ++ generateSudokuConstraints xs
\end{code}

The list comprehension contains the Boolean condition $m \neq n$ to ensure that there will not be an arc $\langle n, n \rangle$ in the constraints, since there will be no assignment that satisfies the constraint $n \neq n$.
Moreover, the list comprehension for the block constraints ensures that variables in the same row or column are ignored, since those have already been taken into account.

The \verb|printSudoku| function takes the list of \verb|Domain|s of a sudoku and prints the (partially) solved sudoku in a readable format using spaces and newlines.
If the list of possible values for a variable only contains one element, this element may be printed; if it does not, then the value of that variable is as of yet undetermined and an underscore is printed to indicate this.

\begin{code}
printSudoku :: [Domain] -> IO ()
printSudoku [] = putStr ""
printSudoku ((n, val@(value:_)):xs) =
  do
    -- put the number there if determined, else _
    putStr (if val == [value] then show value else "_")
    -- put spaces between different blocks
    when ((getVar n) `mod` 3  == 2)  (putStr " ")
    -- put newlines at the end of rows
    when ((getVar n) `mod` 9  == 8)  (putStr "\n")
    -- put extra newlines to vertically separate blocks
    when ((getVar n) `mod` 27 == 26) (putStr "\n")
    do printSudoku xs
-- (to avoid warning about non-exhaustive cases)
printSudoku _ = putStr ""
\end{code}

\todo[inline]{add explanations here}

\begin{code}
-- solves the available sudoku in "sudoku.txt" in the "sudoku/" subdirectory
solveSudokuFromFile :: IO ()
solveSudokuFromFile = do
  sudokuString <- readFile "sudoku/sudoku.txt"
  -- make the string into a list of Ints
  let values = map (Val . digitToInt) sudokuString
  -- solve the sudoku and print it
  do printSudoku $ ac3domain (generateSudokuDomains values) (generateSudokuConstraints (map Var [0..80]))
\end{code}
