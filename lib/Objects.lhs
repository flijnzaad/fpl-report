\section{Objects}

\begin{code}
module Objects where

import CSP
import AC3
import Data.List
import Data.Char

-- Variables: Lines have negative IDs, junctions positive IDs
-- Values:    Lines: -1 = "-", -2 = "+", JunctionIDofOriginVertex = ">"
--         Junction: 0-5 = "L junction", 10-15 = "Fork", 20-23 = "T junction", 30-33 = "Arrow"
-- 

type LineID     = Int
type JunctionID = Int
data Junction   = L LineID LineID JunctionID 
                | Fork LineID LineID LineID JunctionID 
                | T LineID LineID LineID JunctionID 
                | Arrow LineID LineID LineID JunctionID
data Line       = Line JunctionID JunctionID LineID
type Object = ([Junction], [Line])

cube :: Object
cube = ([Arrow (-1) (-2) (-3) 0, L (-3) (-4) 1, Arrow (-4) (-5) (-6) 2, L (-6) (-7) 3, Arrow (-7) (-8) (-9) 4, L (-9) (-1) 5, Fork (-2) (-5) (-8) 6],
        [Line 0 5 (-1), Line 0 6 (-2), Line 0 1 (-3), Line 1 2 (-4), Line 2 6 (-5), Line 2 3 (-6), Line 3 4 (-7), Line 4 6 (-8), Line 4 5 (-9)])

-- The Object type is nice and readable, this function generates the correct csp. 
-- It preserves the LineIDs and JunctionIDs used in the Object def. 
-- !! Lines must have neg. IDs, Junctions positive IDs
cspGeneration :: Object -> Int -> Problem
cspGeneration ([],[]) _ = CSP [] [] []
-- Here we exhaust first the list of Junctions that are passed. The last case exhausts all Lines
cspGeneration ((L i j id):xs, ys) maxID       = CSP (id:vars) ((id,[0..5]):doms) 
    ([
      ((id,i),[(0,var)|var<-otherIDs]++[(1,id),(2,id),(3,-2)]++[(4,var)|var<-otherIDs]++[(5,-1)]),
      ((id,j),[(0,id)]++[(1,var)|var<-otherIDs]++[(2,-2)]++[(3,var)|var<-otherIDs]++[(4,-1),(5,id)])
    ] ++ cons) where 
    CSP vars doms cons = cspGeneration (xs, ys) maxID
    otherIDs = [0..id] ++ [id+1..maxID]
cspGeneration ((Fork i j k id):xs, ys) maxID  = CSP (id:vars) ((id,[10..12]):doms) 
    ([
      ((id,i),[(10,-2),(11,-1)]++[(12,var)|var<-otherIDs]),
      ((id,j),[(10,-2),(11,-1),(12,-1)]),
      ((id,k),[(10,-2),(11,-1),(12,id)])
    ] ++ cons) where 
    CSP vars doms cons = cspGeneration (xs, ys) maxID
    otherIDs = [0..id] ++ [id+1..maxID]
cspGeneration ((T i j k id):xs, ys) maxID     = CSP (id:vars) ((id,[20..23]):doms)
    ([
      ((id,i),[(20,var)|var<-otherIDs]++[(21,var)|var<-otherIDs]++[(22,var)|var<-otherIDs]++[(23,var)|var<-otherIDs]),
      ((id,j),[(20,id),(21,id),(22,id),(23,id)]),
      ((id,k),[(20,var)|var<-otherIDs]++[(21,id),(22,-2),(23,-1)])
    ] ++ cons) where 
    CSP vars doms cons = cspGeneration (xs, ys) maxID
    otherIDs = [0..id] ++ [id+1..maxID]
cspGeneration ((Arrow i j k id):xs, ys) maxID = CSP (id:vars) ((id,[30..32]):doms) 
    ([
      ((id,i),[(30,id),(31,-1),(32,-2)]),
      ((id,j),[(30,var)|var<-otherIDs]++[(31,-2),(32,-1)]),
      ((id,k),[(30,-2),(31,-1),(32,-2)])
    ] ++ cons) where 
    CSP vars doms cons = cspGeneration (xs, ys) maxID
    otherIDs = [0..id] ++ [id+1..maxID]
cspGeneration (xs, (Line i j id):ys) maxID    = CSP (id:vars) ((id,[-2..maxID]):doms) 
    ([
    --pretty poor restriction since any value of a line with an arrow allows both adjacent junctions to be any of those with an arrow
      ((id,i),[(-1,4),(-1,5),(-1,11),(-1,12),(-1,21),(-1,31),(-1,32),
        (-2,3),(-2,4),(-2,10),(-2,22),(-2,30),(-2,31),(-2,32)]++
        [(line,junction) | line<-[0..maxID], junction<-[0,1,2,3,4,5,12,20,21,22,23,24,30]]),
    ((id,j),[(-1,4),(-1,5),(-1,11),(-1,12),(-1,21),(-1,31),(-1,32),
        (-2,3),(-2,4),(-2,10),(-2,22),(-2,30),(-2,31),(-2,32)]++
        [(line,junction) | line<-[0..maxID], junction<-[0,1,2,3,4,5,12,20,21,22,23,24,30]])
    ] ++ cons) where
    CSP vars doms cons = cspGeneration (xs, ys) maxID

\end{code}