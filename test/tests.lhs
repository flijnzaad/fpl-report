\section{Tests}
\label{sec:tests}

We now use the HSpec library to test our AC-3 algorithm.

\begin{code}
module Main where

import CSP
import AC3
import Sudoku
import Objects

import Test.Hspec

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
    it "All lines and junctions of a cube should be labeled" $
      all (\(_,values) -> length values == 1) cubeDomains `shouldBe` True where
        (_, cubeDomains, _) = ac3onObject cube
    it "An impossible object should get an empty domain" $
      fst ac3onObject testfig18 `shouldBe` False
    it "Another object should get labels" $
      all (\(_,values) -> length values == 1) otherObjectDomains `shouldBe` True where
        (_, otherObjectDomains, _) = ac3onObject cube

sudoku :: [Value]
sudoku = [0,5,0,7,0,3,0,0,9,
          0,0,8,0,6,1,0,0,5,
          7,0,3,5,0,4,8,0,0,
          3,0,0,4,0,0,0,9,0,
          2,0,0,0,0,0,0,0,0,
          0,8,0,0,2,0,3,5,0,
          5,1,6,0,4,2,0,0,0,
          0,0,0,0,0,9,5,8,0,
          0,3,0,0,5,7,0,2,0]

cube :: (Object, Outline) -- As in Winston1992 Fig 12.15. Labeled clockwise starting at the upper left junction. Passes our object criteria so should return True.
cube = ([ Arrow (Var 1) (Var 5) (Var 6) (Var 0), L (Var 0) (Var 2) (Var 1), Arrow (Var 3) (Var 1) (Var 6) (Var 2), L (Var 2) (Var 4) (Var 3), 
        Arrow (Var 5) (Var 3) (Var 6) 4, L (Var 4) (Var 0) (Var 5), Fork (Var 0) (Var 2) (Var 4) (Var 6)],
        [ (Var 0, Var 1), (Var 1, Var 2), (Var 2, Var 3), (Var 3, Var 4), (Var 4, Var 5), (Var 5, Var 0) ])

testfig18 :: (Object,Outline) -- As in Fig 12.18. Should return False, since as shown in Winston1992 Fig 12.18 there is no possible labeling of the middle junction.
testfig18 = ([ Arrow (Var 1) (Var 2) (Var 3) (Var 0), L (Var 0) (Var 4) (Var 1), L (Var 7) (Var 0) (Var 2), Fork (Var 0) (Var 8) (Var 9) (Var 3),
             Arrow (Var 5) (Var 1) (Var 6) (Var 4), L (Var 4) (Var 7) (Var 5), Fork (Var 8) (Var 7) (Var 4) (Var 6), 
             Arrow (Var 2) (Var 5) (Var 6) (Var 7), Arrow (Var 3) (Var 6) (Var 9) (Var 8), L (Var 3) (Var 8) (Var 9)],
             [ (Var 0, Var 1), (Var 1, Var 4), (Var 4, Var 5), (Var 5, Var 7), (Var 7, Var 2), (Var 2, Var 0) ])

testfig12A :: (Object,Outline) -- Test including a T junction. This is from the perspective of observer A in Fig 12.12 in Winston1992.
testfig12A = ([ Arrow (Var 1) (Var 8) (Var 9) (Var 0), L (Var 0) (Var 2) (Var 1), Arrow (Var 4) (Var 1) (Var 3) (Var 2), L (Var 4) (Var 2) (Var 3),
              T (Var 3) (Var 5) (Var 2) (Var 4), Arrow (Var 6) (Var 4) (Var 9) (Var 5), L (Var 5) (Var 7) (Var 6), 
              Arrow (Var 8) (Var 6) (Var 9) (Var 7), L (Var 7) (Var 0) (Var 8), Fork (Var 0) (Var 7) (Var 5) (Var 9) ],
              [ (Var 0, Var 1), (Var 1, Var 2), (Var 2, Var 4), (Var 4, Var 5), (Var 5, Var 6), (Var 6, Var 7), (Var 7, Var 8), (Var 8, Var 0) ])
\end{code}

Moreover, the algorithm and sudoku implementation can be tested manually by running \verb|python3 generate_sudoku.py| in the \verb|sudoku| subdirectory to generate a new sudoku, and in the main directory running \verb|stack ghci| and then \verb|ac3SudokuFromFile|.
