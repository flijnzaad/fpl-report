\section{Constraint satisfaction problems}\label{sec:CSP}

\begin{code}
{-# LANGUAGE
  GeneralizedNewtypeDeriving
#-}

module CSP where

-- type definitions based on formal definition of CSP in book
newtype Variable = Var { getVar :: Int } deriving (Eq, Show, Ord, Num)
newtype Value    = Val { getVal :: Int } deriving (Eq, Ord, Num)
instance Show Value where
  show x = show (getVal x)
type Domain      = (Variable, [Value])
type Arc         = (Variable, Variable)
type Constraint  = ( Arc, [(Value, Value)] )
data Problem     = CSP { variables   :: [Variable]
                       , domains     :: [Domain]
                       , constraints :: [Constraint] }
\end{code}
