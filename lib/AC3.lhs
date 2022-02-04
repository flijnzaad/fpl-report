\section{The AC-3 algorithm}\label{sec:ac3}

This functional implementation of the AC-3 algorithm is based on the imperative pseudocode in \cite[p.~209]{AIMA}.

The implementation makes use of the auxiliary function \verb|getVarDomain|.
When given a variable and a list of domains, it returns the domain of that variable. It performs \verb|lookup| on the list of domains and returns not only the second argument of the relevant tuple (which is a list of values), but the whole tuple (the whole domain). Moreover, it uses \verb|fromJust| to strip the domain of its \verb|Just|. Generally, this is not safe to do, but in this implementation we are certain that the \verb|lookup| will never return \verb|Nothing|: the use of \verb|getVarDomain| is restricted to cases where we are certain that the variable to look up is actually present in the list.

\begin{code}
module AC3 where

import CSP
import Data.List
import Data.Maybe -- for using "fromJust"

getVarDomain :: Variable -> [Domain] -> Domain
getVarDomain var doms = let dom = fromJust $ lookup var doms in (var, dom)
\end{code}

The \verb|ac3| function takes as input a tuple containing the full CSP, a Boolean flag and a queue of constraints. The Boolean flag is the true or false that the algorithm returns as described in \cref{sec:arc-consistency}.
The queue of constraints more or less functions as a to-do list, containing the arcs of which the consistency needs to be checked still.
When first calling the function, this queue contains all constraints of the CSP; when the queue of constraints is empty, the CSP is arc-consistent.
If at any point during the recursion the Boolean flag is set to false, this means that the domain of a variable is empty and the CSP has no solution. In this case, there is no point in continuing the recursion, so it is halted.

\begin{code}
ac3 :: (Problem, Bool, [Constraint]) -> (Problem, Bool, [Constraint])
ac3 (p, False, _) = (p, False, [])
ac3 (p, True, []) = (p, True,  [])
\end{code}

In the recursive case, the first constraint $C = \langle \langle x, y \rangle, R \rangle$ in the constraint queue is considered.
The arc $\langle x, y \rangle$ of $C$ is then `revised' based on this constraint:
the domain of $x$ is restricted to those values that satisfy the constraint $C$. More specifically, the new domain for $x$, \verb|newXDomain|, is such that those $x' \in D_x$ are kept for which $\exists y' \in D_y$ such that $\langle x', y' \rangle \in R$.

If this revision did not change the domain of $x$ (i.e., $\langle x, y \rangle$ was already arc-consistent), then the recursion continues with the CSP unchanged, with the flag still set to \verb|True| (because no domains were changed) and with rest of the queue of constraints.

If the revision \emph{did} change the domain of $x$, then this may cause changes in the domains of the `neighbors' of $x$: the arcs of which $x$ is the second argument. To propagate these changes, the neighbors of $x$ are added to the queue (\verb|newQueue|). The domain list is updated (\verb|newDoms|) by deleting the old domain of $x$ from it and adding the new domain of $x$ to it. The new Boolean flag is whether or not $x$'s new domain is empty.

\begin{code}
ac3 (p@(CSP doms cons), True, ((x, y), rel):queue) =
  if getVarDomain x doms == newXDomain
    then ac3 (p, True, queue)
    else ac3 (CSP newDoms cons, not $ null $ snd newXDomain, newQueue)
  where
    newXDomain = ( x, [ x' | x' <- xvals, any (\y' -> (x', y') `elem` rel) yvals ] ) where
      xvals = snd $ getVarDomain x doms
      yvals = snd $ getVarDomain y doms
    newDoms  = newXDomain : delete (getVarDomain x doms) doms
    newQueue = queue ++ filter (\(arc, _) -> snd arc == x) cons
\end{code}

Since the \verb|ac3| function outputs a full CSP, it is useful for practical applications to have a wrapper function that calls \verb|ac3| and only outputs the list of domains. Moreover, it is useful to have this list be sorted, since during execution of the AC-3 algorithm, the list of domains has been scrambled.

\begin{code}
arcConsistentDomain :: Problem -> [Domain]
arcConsistentDomain problem@(CSP _ cons) =
  if succeeded
    then
      sortBy (\(a,_) (b,_) -> compare a b) y
    else []
  where
    (CSP y _, succeeded, _) = ac3 (problem, True, cons)
\end{code}
