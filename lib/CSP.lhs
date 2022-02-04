\section{Constraint satisfaction problems}\label{sec:CSP}

A constraint satisfaction problem (CSP) is a triple $\langle X, D, C \rangle $ where
\begin{itemize}
    \item $X$ is a set of variables $\{ x_1, \ldots, x_n \}$;
    \item $D$ is a set of domains $\{ D_1, \ldots, D_n \}$. Each domain is a set of values for a variable;
    \item $C$ is a set of constraints on the domains of the variables.
\end{itemize}

A constraint is a pair $\langle scope,\ relation \rangle $, where the $scope$ is a tuple of the variables that participate in the constraint, and the $relation$ is a set of tuples determining the allowed combinations of values for the variables in the $scope$.

Any constraint with a finite scope can be reduced to a set of binary constraints \cite[p. 206]{AIMA}; this greatly simplifies the representation of constraints.
We call the scope of a binary constraint an \emph{arc}.

In our Haskell implementation of CSPs, we define \verb|type|s and \verb|newtype|s for variables, values, domains, arcs, constraints and CSPs.
In the formal definition of a CSP, which domains correspond to which variables is indicated by their subscripted indices matching.
In our implementation, we chose to represent a domain as a tuple of the variable it pertains to, together with the list of possible values, e.g. $\langle x_1, D_1 \rangle $.
Since then the set of domains already contains the variables as well, there is no need for including the set of variables in the definition of a CSP.
Therefore, in our implementation a CSP is just the pair $\langle D, C \rangle $.

\begin{code}
{-# LANGUAGE
  GeneralizedNewtypeDeriving
#-}

module CSP where

import Data.List -- for using "sort"

newtype Variable = Var { getVar :: Int } deriving (Eq, Ord, Num)
instance Show Variable where
  show x = show (getVar x)
newtype Value    = Val { getVal :: Int } deriving (Eq, Ord, Num)
instance Show Value where
  show x = show (getVal x)
newtype Domain   = Dom (Variable, [Value]) deriving (Show)
instance Eq Domain where
  (Dom x) == (Dom y) = (fst x == fst y) && (nub (sort (snd x)) == nub (sort (snd y)))
type Arc         = (Variable, Variable)
type Constraint  = ( Arc, [(Value, Value)] )
data Problem     = CSP { domains     :: [Domain]
                       , constraints :: [Constraint] } deriving Show
\end{code}
