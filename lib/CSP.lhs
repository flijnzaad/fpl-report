\section{Constraint satisfaction problems}\label{sec:CSP}

A constraint satisfaction problem (CSP) is a triple $\langle X, D, C \rangle $ where
\begin{itemize}
    \item $X$ is a set of variables $\{ x_1, \ldots, x_n \}$;
    \item $D$ is a set of domains $\{ D_1, \ldots, D_n \}$. Each domain is a set of possible values for a variable;
    \item $C$ is a set of constraints on the domains of the variables.
\end{itemize}

A constraint is a pair $\langle scope,\ relation \rangle $, where the $scope$ is a tuple of the variables that participate in the constraint, and the $relation$ is a set of tuples determining the allowed combinations of values for the variables in the $scope$.

Any constraint with a finite scope can be reduced to a set of binary constraints \cite[p. 206]{AIMA}; this greatly simplifies the representation of constraints, especially for the purposes of implementing algorithms.
We call the scope of a binary constraint an \emph{arc}.

In our Haskell implementation of CSPs, we define \verb|type|s and \verb|newtype|s for variables, values, domains, arcs, constraints and CSPs.
In the formal definition of a CSP, a domain $D_i \in D$ corresponds to the variable $x_i \in X$; i.e., which domain corresponds to which variable is indicated by their subscripted indices matching.
In our implementation, we chose to represent a domain as a tuple of the variable it pertains to, together with the list of possible values, e.g. $\langle x_1, D_1 \rangle $.
Since then the set of domains already contains the variables as well, there is no need for including the set of variables in the definition of a CSP.
Therefore, in our implementation a CSP is just the pair $\langle D, C \rangle $.

\begin{code}
{-# LANGUAGE
  GeneralizedNewtypeDeriving
#-}

module CSP where

newtype Variable = Var { getVar :: Int } deriving (Eq, Ord, Num)
instance Show Variable where
  show x = show (getVar x)
newtype Value    = Val { getVal :: Int } deriving (Eq, Ord, Num)
instance Show Value where
  show x = show (getVal x)
type Domain      = (Variable, [Value])
type Arc         = (Variable, Variable)
type Constraint  = ( Arc, [(Value, Value)] )
data Problem     = CSP { domains     :: [Domain]
                       , constraints :: [Constraint] } deriving Show
\end{code}
