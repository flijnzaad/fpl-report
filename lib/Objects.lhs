\section{Objects}

\begin{code}
module Objects where

import CSP
import AC3
import Data.List
import Data.Tuple
import Data.Ord


-- Values:    Lines: 0 = "-", 1 = "+", 2 = "incoming <", 3 = "outgoing >"
--         Junction: 0-5 = "L junction", 10-15 = "Fork", 20-23 = "T junction", 30-33 = "Arrow"

type LineID     = Variable
type JunctionID = Variable
-- We are going to give lines negative IDs and junctions non-negative IDs
data Junction   = L JunctionID JunctionID JunctionID  
                | Fork JunctionID JunctionID JunctionID JunctionID 
                | T JunctionID JunctionID JunctionID JunctionID 
                | Arrow JunctionID JunctionID JunctionID JunctionID
\end{code}

For the labeling of a Junction we use the orientation used in Figure 12.14 of \cite[p.259]{winston1992}, and first label the JunctionID in the rightmost direction. 
Then we go anticlockwise (positive arc) labeling the next (two). The final JunctionID is that of the Junction itself. 
So e.g. Arrow (Var 8) (Var 4) (Var 5) (Var 1), would be an Arrow junction with JunctionID Var 1, and the junction in the direction of the "shaft" of the arrow has JunctionID Var 4

\begin{code}
type Object     = [Junction] 
type Outline    = [(JuctionID,JunctionID)] -- Outline

-- We are going to work with 4-tuples of this type, these projections make that a bit easier
myfst, myfrth :: (JunctionID,LineID,LineID,JunctionID) -> JunctionID
mysnd, mytrd  :: (JunctionID,LineID,LineID,JunctionID) -> LineID

myfst  (a, _, _, _) = a
mysnd  (_, b, _, _) = b
mytrd  (_, _, c, _) = c
myfrth (_, _, _, d) = d

-- cycle through list of junctions, and add two lines between all connected junctions. 
lineGeneration :: Object -> LineID -> [(JunctionID,LineID,LineID,JunctionID)]
lineGeneration [] _                         = []
lineGeneration ((L i j juncID):xs)       lineID = lineCreation juncID [i,j] lineID   ++ lineGeneration xs (Var (getVar lineID-2*length (lineCreation juncID [i,j]   lineID)))
lineGeneration ((Fork i j k juncID):xs)  lineID = lineCreation juncID [i,j,k] lineID ++ lineGeneration xs (Var (getVar lineID-2*length (lineCreation juncID [i,j,k] lineID)))
lineGeneration ((T i j k juncID):xs)     lineID = lineCreation juncID [i,j,k] lineID ++ lineGeneration xs (Var (getVar lineID-2*length (lineCreation juncID [i,j,k] lineID)))
lineGeneration ((Arrow i j k juncID):xs) lineID = lineCreation juncID [i,j,k] lineID ++ lineGeneration xs (Var (getVar lineID-2*length (lineCreation juncID [i,j,k] lineID)))

lineCreation :: JunctionID -> [JunctionID] -> LineID -> [(JunctionID,LineID,LineID,JunctionID)]
lineCreation _ [] _ = []
lineCreation juncID (i:is) lineID = if getVar juncID < getVar i then (juncID, lineID, Var (getVar lineID-1), i):lineCreation juncID is (Var (getVar lineID-2)) else lineCreation juncID is lineID

junctionConstraints :: Object -> [(JunctionID,LineID,LineID,JunctionID)] -> ([Domain], [Constraint])
junctionConstraints [] _ = ([],[])
-- We consider the junctions one by one. We have already chosen the IDs of the lines between two junctions, and we use to the lineto lambda to find the correct ID
junctionConstraints ((L i j juncID):xs) lineInfo       = ((juncID,[Val 0, Val 1, Val 2, Val 3, Val 4, Val 5]):doms,[
  ((juncID,lineto i), [(Val 0,Val 2), (Val 1,Val 3), (Val 2,Val 3), (Val 3,Val 1), (Val 4,Val 2), (Val 5,Val 0)]),
  ((juncID,lineto j), [(Val 0,Val 3), (Val 1,Val 2), (Val 2,Val 1), (Val 3,Val 2), (Val 4,Val 0), (Val 5,Val 5)])
  ] ++ cons) where
  (doms, cons) = junctionConstraints xs lineInfo
  lineto = \n -> head (map mysnd (filter (\line -> myfst line == juncID && myfrth line == n) lineInfo) ++ map mytrd (filter (\line -> myfst line == n && myfrth line == juncID) lineInfo))
    -- Either (juncID, correctline, _ , i) or (i, _, correctline, juncID) appears in the output of lineGeneration, so head will always work.
junctionConstraints ((Fork i j k juncID):xs) lineInfo  = ((juncID,[Val 10, Val 11, Val 12]):doms,[
  ((juncID,lineto i), [(Val 10,Val 1), (Val 11,Val 0), (Val 12,Val 0), (Val 12,Val 2), (Val 12,Val 3)]),
  ((juncID,lineto j), [(Val 10,Val 1), (Val 11,Val 0), (Val 12,Val 0), (Val 12,Val 2), (Val 12,Val 3)]),
  ((juncID,lineto k), [(Val 10,Val 1), (Val 11,Val 0), (Val 12,Val 0), (Val 12,Val 2), (Val 12,Val 3)])
  ] ++ cons) where
  (doms, cons) = junctionConstraints xs lineInfo
  lineto = \n -> head (map mysnd (filter (\line -> myfst line == juncID && myfrth line == n) lineInfo) ++ map mytrd (filter (\line -> myfst line == n && myfrth line == juncID) lineInfo))
junctionConstraints ((T i j k juncID):xs) lineInfo      = ((juncID,[Val 20, Val 21, Val 22,Val 23]):doms,[
  ((juncID,lineto i), [(Val 20,Val 2), (Val 21,Val 2), (Val 22,Val 3), (Val 23,Val 2)]),
  ((juncID,lineto j), [(Val 20,Val 3), (Val 21,Val 3), (Val 22,Val 3), (Val 23,Val 3)]),
  ((juncID,lineto k), [(Val 20,Val 2), (Val 21,Val 3), (Val 22,Val 1), (Val 23,Val 0)])
  ] ++ cons) where
  (doms, cons) = junctionConstraints xs lineInfo
  lineto = \n -> head (map mysnd (filter (\line -> myfst line == juncID && myfrth line == n) lineInfo) ++ map mytrd (filter (\line -> myfst line == n && myfrth line == juncID) lineInfo))
junctionConstraints ((Arrow i j k juncID):xs) lineInfo  = ((juncID,[Val 30, Val 31, Val 32]):doms,[
  ((juncID,lineto i), [(Val 30,Val 3), (Val 31,Val 1), (Val 32,Val 0)]),
  ((juncID,lineto j), [(Val 30,Val 2), (Val 31,Val 1), (Val 32,Val 0)]),
  ((juncID,lineto k), [(Val 30,Val 1), (Val 31,Val 0), (Val 32,Val 1)])
  ] ++ cons) where
  (doms, cons) = junctionConstraints xs lineInfo
  lineto = \n -> head (map mysnd (filter (\line -> myfst line == juncID && myfrth line == n) lineInfo) ++ map mytrd (filter (\line -> myfst line == n && myfrth line == juncID) lineInfo))

-- The Object type is nice and relatively readable, this function generates the correct csp. 
-- It preserves the JunctionIDs used in the Object def.
-- !! Junctions must have positive IDs

cspGenerator :: Object -> Problem
cspGenerator object = CSP (allJuncDomains ++ allLineDomains) allConstraints where

  lineInfo                              = lineGeneration object (Var (-1)) -- first line gets variable -1
  (allJuncDomains, allJuncConstraints)  = junctionConstraints object lineInfo
  allLineDomains      = concat [ [(Var (2*line+1),[Val 0, Val 1, Val 2, Val 3]),(Var (2*line),[Val 0, Val 1, Val 2, Val 3])] | line <- [-length lineInfo..(-1)] ]
  allLineConstraints  = [ ((Var (2*line)+1, Var (2*line)), [(Val 0,Val 0), (Val 1,Val 1), (Val 2,Val 3), (Val 3,Val 2)]) | line <- [-length lineInfo..(-1)] ]
  allConstraints = allJuncConstraints ++ allLineConstraints ++ map reflectConstraint allJuncConstraints ++ map reflectConstraint allLineConstraints
  reflectConstraint = \(arc, rel) -> (swap arc, map swap rel) -- we need arcs to be included in both directions. 


-- Restrict the domains of the outline. A line with arrow leaving a vertex will get Val 3, an incoming line gets Val 2.
setOutlineArrows :: [Domain] -> [(JunctionID,LineID,LineID,JunctionID)] -> Outline -> [Domain]
setOutlineArrows doms _ []              = doms
setOutlineArrows doms lineInfo ((x,y):xs) 
                  -- the following is safe if our list of border junctions is actually a path on the object, since:
                  -- if x<y we will have (x,i,i-1,y) in our lineInfo, otherwise we will have (y,i,i-1,x) as an element.
                              | getVar x < getVar y       = let Just (_,i,_,_) = find (\line -> myfst line == x && myfrth line == y) lineInfo in 
                                  (i,[Val 3]):(Var (getVar i-1),[Val 2]):
                                    delete (i,[Val 0, Val 1, Val 2, Val 3]) (delete (Var (getVar i-1),[Val 0, Val 1, Val 2, Val 3]) (setOutlineArrows doms lineInfo xs))
                              | otherwise = let Just (_,i,_,_) = find (\line -> myfst line == y && myfrth line == x) lineInfo in 
                                  (i,[Val 2]):(Var (getVar i-1),[Val 3]):
                                    delete (i,[Val 0, Val 1, Val 2, Val 3]) (delete (Var (getVar i-1),[Val 0, Val 1, Val 2, Val 3]) (setOutlineArrows doms lineInfo xs))

cube :: (Object, Outline) -- Passes our object criteria so should return True, junction Var 6 is the middle Fork, should get Val 10
cube = ([ Arrow (Var 1) (Var 5) (Var 6) (Var 0), L (Var 0) (Var 2) (Var 1), Arrow (Var 3) (Var 1) (Var 6) (Var 2), L (Var 2) (Var 4) (Var 3), 
        Arrow (Var 5) (Var 3) (Var 6) 4, L (Var 4) (Var 0) (Var 5), Fork (Var 0) (Var 2) (Var 4) (Var 6)],
        [ (Var 0, Var 1), (Var 1, Var 2), (Var 2, Var 3), (Var 3, Var 4), (Var 4, Var 5), (Var 5, Var 0) ])

testfig18 :: (Object,Outline) -- Should return False, since Domain of center junction Var 9 will be empty
testfig18 = ([ Arrow (Var 1) (Var 2) (Var 3) (Var 0), L (Var 0) (Var 4) (Var 1), L (Var 7) (Var 0) (Var 2), Fork (Var 0) (Var 8) (Var 9) (Var 3),
             Arrow (Var 5) (Var 1) (Var 6) (Var 4), L (Var 4) (Var 7) (Var 5), Fork (Var 8) (Var 7) (Var 4) (Var 6), 
             Arrow (Var 2) (Var 5) (Var 6) (Var 7), Arrow (Var 3) (Var 6) (Var 9) (Var 8), L (Var 3) (Var 8) (Var 9)],
             [ (Var 0, Var 1), (Var 1, Var 4), (Var 4, Var 5), (Var 5, Var 7), (Var 7, Var 2), (Var 2, Var 0) ])

testfig12A :: (Object,Outline) -- Test including a T junction. Should return True. 
testfig12A = ([ Arrow (Var 1) (Var 8) (Var 9) (Var 0), L (Var 0) (Var 2) (Var 1), Arrow (Var 4) (Var 1) (Var 3) (Var 2), L (Var 4) (Var 2) (Var 3),
              T (Var 3) (Var 5) (Var 2) (Var 4), Arrow (Var 6) (Var 4) (Var 9) (Var 5), L (Var 5) (Var 7) (Var 6), 
              Arrow (Var 8) (Var 6) (Var 9) (Var 7), L (Var 7) (Var 0) (Var 8), Fork (Var 0) (Var 7) (Var 5) (Var 9) ],
              [ (Var 0, Var 1), (Var 1, Var 2), (Var 2, Var 4), (Var 4, Var 5), (Var 5, Var 6), (Var 6, Var 7), (Var 7, Var 8), (Var 8, Var 0) ])

ac3input :: (Object, Outline) -> (Problem, Bool, [Constraint])
ac3input object outline = let CSP doms cons = cspGenerator object in (CSP (setOutlineArrows doms (lineGeneration object (-1)) outline) cons, True, cons)

-- Output of ac3 to Int instead of Var
ac3onObject :: (Object, Outline) -> (Bool,[(Int, [Value])],[((Int,Int),[(Value,Value)])])
ac3onObject (object, outline) = (bool, map (\(var,vals) -> (getVar var, vals))  doms, map (\((x,y),pairs) -> ((getVar x, getVar y),pairs)) cons)
  where (CSP doms cons, bool, _) = ac3 (ac3input (object, outline))
\end{code}