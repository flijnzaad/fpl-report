\section{Constraint satisfaction problems}

\begin{code}
module CSP where

import Data.List
import Data.Char

-- type definitions based on formal definition of CSP in book
type Variable   = Int 
type Value      = Int
type Domain     = (Variable, [Value])
type Arc        = (Variable, Variable)
type Constraint = ( Arc, [(Value, Value)] )
data Problem    = CSP { vars :: [Variable]
                      , doms :: [Domain]
                      , cons :: [Constraint] } deriving Show
\end{code}
