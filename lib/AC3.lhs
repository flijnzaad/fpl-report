\section{The AC-3 algorithm}\label{sec:ac3}

This functional implementation of the AC-3 algorithm is based on the imperative pseudocode in \cite[p.~209]{AIMA}.

The \verb|ac3| function takes as input a tuple containing the full CSP, a Boolean flag and a queue of constraints. The Boolean flag is the true or false that the algorithm returns as described in \cref{sec:arc-consistency}.
The queue of constraints more or less functions as a to-do list, containing the arcs of which the consistency needs to be checked still.
When first calling the function, this queue contains all constraints of the CSP; when the queue of constraints is empty, the CSP is arc-consistent.
If at any point during the recursion the Boolean flag is set to false, this means that the domain of a variable is empty and the CSP has no solution. In this case, there is no point in continuing the recursion, so it is halted.

\begin{code}
module AC3 where

import CSP
import Data.List
ac3 :: (Problem, Bool, [Constraint]) -> (Problem, Bool, [Constraint])
ac3 (p, False, _) = (p, False, [])
ac3 (p, True, []) = (p, True,  [])
\end{code}

\begin{code}
ac3 (p@(CSP doms cons), True, ((varX, varY), rel):xs) =
  if unsafeLookup varX doms == newXDomain
    -- if after revising, the domain of x stays the same,
    -- continue with the next arc in the queue and pass whether newXDomain is nonempty
    then ac3 (p, not $ null $ snd newXDomain, xs)
    -- if the domain of x has changed, need to add x's neighbors to queue
    else ac3 (CSP newDoms cons, True, newQueue)
  where
    newXDomain = (varX, [ x | x <- xvals, any (\y -> (x, y) `elem` rel) yvals ]) where
      xvals = snd $ unsafeLookup varX doms
      yvals = snd $ unsafeLookup varY doms
    -- delete x's old domain and add x's new domain to the list of domains
    newDoms    = newXDomain : delete (unsafeLookup varX doms) doms
    -- append to the arc queue xs the neighbors of x by filtering on (_, x)
    newQueue   = xs ++ filter (\(arc, _) -> snd arc == varX) cons

-- perform lookup and drop the Maybe
unsafeLookup :: Variable -> [Domain] -> Domain
unsafeLookup x v = let (Just y) = lookup x v in (x,y)

-- since ac3 outputs a CSP including all of the constraints, we use this to return only the domain. Note that the problem has a unique solution if all problems have size 1
ac3domain :: [Domain] -> [Constraint] -> [Domain]
ac3domain doms cons = let (CSP y _, _, _) = ac3 (CSP doms cons, True, cons) in sortBy (\(a,_) (b,_) -> compare a b) y
-- do something about returning False!
\end{code}
