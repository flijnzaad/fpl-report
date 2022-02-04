\section{Tests}
\label{sec:tests}

We now use the HSpec library to test our AC-3 algorithm.

\begin{code}
module Main where

import CSP
import AC3
import Sudoku

import Test.Hspec
\end{code}

The following uses the HSpec library to define different tests.
Note that the first test is a specific test with fixed inputs.

\begin{code}
main :: IO ()
main = hspec $ do
  describe "AC-3" $ do
    it "A test sudoku should be the same when run through AC-3 once as twice" $
      (arcConsistentDomain sudokuCSP) `shouldBe`
      (arcConsistentDomain (CSP (arcConsistentDomain sudokuCSP) sudokuCons)) where
        sudokuCons :: [Constraint]
        sudokuCons = genSudokuCons (map Var [0..80])
        sudokuCSP :: Problem
        sudokuCSP = CSP (genSudokuDoms sudoku) sudokuCons

sudoku :: [Value]
sudoku = [0,5,0,7,0,3,0,0,9,0,0,8,0,6,1,0,0,5,7,0,3,5,0,4,8,0,0,3,0,0,4,0,0,0,9,0,2,0,0,0,0,0,0,0,0,0,8,0,0,2,0,3,5,0,5,1,6,0,4,2,0,0,0,0,0,0,0,0,9,5,8,0,0,3,0,0,5,7,0,2,0]
\end{code}

To run the tests, use \verb|stack test|.

To also find out which part of your program is actually used for these tests,
run \verb|stack clean && stack test --coverage|. Then look for ``The coverage
report for ... is available at ... .html'' and open this file in your browser.
See also: \url{https://wiki.haskell.org/Haskell_program_coverage}.
