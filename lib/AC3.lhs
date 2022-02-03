\section{The AC-3 algorithm}

This functional implementation of the AC-3 algorithm is based on the imperative pseudocode in \cite[p.~209]{AIMA}.

\begin{code}
module AC3 where

import CSP
import Data.List
-- implementation of the AC-3 function, recursive version of the pseudocode in
-- the book; calls `revise` helper function. the book version passes a queue
-- of arcs; we use a list of constraints, since those contain the arcs
ac3 :: (Problem, Bool, [Constraint]) -> (Problem, Bool, [Constraint])
-- if the Bool flag is False, the CSP has no solution, so stop the recursion
ac3 (p, False, _) = (p, False, [])
-- if the arc queue is empty, stop the recursion and return True
ac3 (p, True, []) = (p, True,  [])
-- else, perform body of the `while` loop
ac3 (p@(CSP vars doms cons), True, ((varX, varY), rel):xs) =
  if unsafeLookup varX doms == newXDomain
    -- if after revising, the domain of x stays the same,
    -- continue with the next arc in the queue and pass whether newXDomain is nonempty
    then ac3 (p, not $ null $ snd newXDomain, xs)
    -- if the domain of x has changed, need to add x's neighbors to queue
    else ac3 (CSP vars newDoms cons, True, newQueue)
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
ac3domain :: [Variable] -> [Domain] -> [Constraint] -> [Domain]
ac3domain vars doms cons = let (CSP _ y _, _, _) = ac3 (CSP vars doms cons, True, cons) in sortBy (\(a,_) (b,_) -> compare a b) y
\end{code}
