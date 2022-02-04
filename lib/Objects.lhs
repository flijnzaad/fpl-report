\section{Objects}

This chapter is based on a task described in Chapter 12 of \cite{winston1992}: \emph{Symbolic Constraints and Propagation}. It describes how a computer can interpret a 2D image of lines as a 3D object.
The task consists of deciding whether a given set of lines define an object without curved faces and of classifying the lines as either convex, concave or boundary edges.
There is no shading involved, but an object can partially block the view of itself or other objects.
The vertices at which the lines meet are restricted in such a way that they have a maximum of three adjacent faces. 
The viewpoint of the ``observer" doesn't give any ``illusions" like lines or vertices that overlap.
This is not a heavy restriction, since in a real world scenario one could accomplish this by a small change of prespective.

\begin{code}
module Objects where

import CSP
import AC3
import Data.List
import Data.Tuple

type LineID     = Variable
type JunctionID = Variable
-- We are going to give lines negative IDs and junctions non-negative IDs

data Junction   = L JunctionID JunctionID JunctionID  
                | Fork JunctionID JunctionID JunctionID JunctionID 
                | T JunctionID JunctionID JunctionID JunctionID 
                | Arrow JunctionID JunctionID JunctionID JunctionID

type Object     = [Junction] 
type Outline    = [(JunctionID,JunctionID)]
\end{code}

When a real object has its lines labeled correctly, there are only 18 possible junctions, as identified in Figure 12.14 of \cite[p.259]{winston1992}.
Since classifying a junction as either an L, Fork, T or Arrow junction can be done without any global information, we will assume that we know that property of each junction.
This could also be done as a preprocessing step by another script, but that is not part of our task here.

For the labeling of a junction we use the orientation used in Figure 12.14 of \cite[p.259]{winston1992}, and first label the JunctionID in the rightmost direction. 
We then go anticlockwise labeling the next (two) junction(s). The final JunctionID is that of the junction itself. 
So e.g. Arrow (Var 8) (Var 4) (Var 5) (Var 1), would be an Arrow junction with JunctionID Var 1, and the junction in the direction of the "shaft" of the arrow has JunctionID Var 4. 
For a complete explanation the reader should read the beforementioned chapter. 

To solve this task we add not one, but two variables for lines between two connected junctions. These lines are connected to eachother and one to each junction at the endpoints.
When one of these lines is labeled as a "+" (convex edge) or "-" (concave), the other will be as well. For the boundary arrows we use a different label for each: 
when an arrow should go from junction 1 to junction 2, we label the line variable adjacent to junction 1 as ``outgoing" and label the other line variable as ``incoming".

We can use arc consistency to solve most of these problems: for many junctions knowledge of the labels of two of its adjacent lines will tell us which junction we are dealing with, and then we know the label of the other line.
Propagating this process will add more and more labels, until (hopefully) all lines are labeled. 

\begin{code}
-- We are going to work with 4-tuples of this type, these projections make that a bit easier
myfst, myfrth :: (JunctionID,LineID,LineID,JunctionID) -> JunctionID
mysnd, mytrd  :: (JunctionID,LineID,LineID,JunctionID) -> LineID
myfst  (a, _, _, _) = a
mysnd  (_, b, _, _) = b
mytrd  (_, _, c, _) = c
myfrth (_, _, _, d) = d

-- The reader might find some of the following functions a bit too resembling of imperative programming.
-- We would have to agree, but like to point out that most perfecting work has gone into the AC-3 algorithm.

-- Cycle through list of junctions, and add two lines between all connected junctions. 
lineGeneration :: Object -> LineID -> [(JunctionID,LineID,LineID,JunctionID)]
lineGeneration [] _                         = []
lineGeneration ((L i j juncID):xs)       lineID = lineCreation juncID [i,j] lineID   ++ lineGeneration xs (Var (getVar lineID-2*length (lineCreation juncID [i,j]   lineID)))
lineGeneration ((Fork i j k juncID):xs)  lineID = lineCreation juncID [i,j,k] lineID ++ lineGeneration xs (Var (getVar lineID-2*length (lineCreation juncID [i,j,k] lineID)))
lineGeneration ((T i j k juncID):xs)     lineID = lineCreation juncID [i,j,k] lineID ++ lineGeneration xs (Var (getVar lineID-2*length (lineCreation juncID [i,j,k] lineID)))
lineGeneration ((Arrow i j k juncID):xs) lineID = lineCreation juncID [i,j,k] lineID ++ lineGeneration xs (Var (getVar lineID-2*length (lineCreation juncID [i,j,k] lineID)))

lineCreation :: JunctionID -> [JunctionID] -> LineID -> [(JunctionID,LineID,LineID,JunctionID)]
lineCreation _ [] _ = []
lineCreation juncID (i:is) lineID = if getVar juncID < getVar i then (juncID, lineID, Var (getVar lineID-1), i):lineCreation juncID is (Var (getVar lineID-2)) else lineCreation juncID is lineID
\end{code}

We have created two line variables between every pair of connected junctions. The constraint corresponding to an arc of a junction and adjacent line depends on the type of the junction and the direction of that line in it. Hence the pretty convoluted function.

\begin{code}
-- Values:  Lines: 0 = "-", 1 = "+", 2 = "incoming <", 3 = "outgoing >"
--    Junctions: 0-5 = "L junction", 10-12 = "Fork", 20-23 = "T junction", 30-33 = "Arrow", ordered from high to low as in Figure 12.15.
-- Val 12 corresponds to the third, fourth and fifth Fork junction, since the rotational symmetry of the Fork junction makes them indistinguishable.
-- This gives the most lenient constraint. We hypothesize that this will result in some unsolved cases, which may be resolved by imposing constraints between the lines, or by path consistency.
junctionConstraints :: Object -> [(JunctionID,LineID,LineID,JunctionID)] -> ([Domain], [Constraint])
junctionConstraints [] _ = ([],[])
-- We consider the junctions one by one. We have already chosen the IDs of the lines between two connected junctions, so we use to the lineto lambda to find that correct ID
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

-- The Object type is nice and relatively readable, this function generates the correct (not so readable) CSP. 
-- It preserves the JunctionIDs used in the Object def. Be aware! Junctions must have non-negative IDs to prevent becoming indistinguishable from lines.
cspGenerator :: Object -> Problem
cspGenerator object = CSP (allJuncDomains ++ allLineDomains) allConstraints where
  lineInfo                              = lineGeneration object (Var (-1)) -- Lines have negative variables, starting with Var -1
  (allJuncDomains, allJuncConstraints)  = junctionConstraints object lineInfo
  allLineDomains                        = concat [ [(Var (2*line+1),[Val 0, Val 1, Val 2, Val 3]),(Var (2*line),[Val 0, Val 1, Val 2, Val 3])] | line <- [-length lineInfo..(-1)] ]
  allLineConstraints                    = [ ((Var (2*line)+1, Var (2*line)), [(Val 0,Val 0), (Val 1,Val 1), (Val 2,Val 3), (Val 3,Val 2)]) | line <- [-length lineInfo..(-1)] ]
  allConstraints                        = allJuncConstraints ++ allLineConstraints ++ map reflectConstraint allJuncConstraints ++ map reflectConstraint allLineConstraints
  reflectConstraint                     = \(arc, rel) -> (swap arc, map swap rel) -- we need arcs to be included in both directions. 
\end{code}

An object will now have a corresponding CSP. Just like the Sudoku example is initialized by knowing some of the values, the line-drawing task is initialized by knowing the outline of an object.
This outline can in most/all? cases be found by tracing the edge of a face from junction to junction, and finding an impossible junction if we picked an incorrect initial line/face. 
But there might even be several disconnected objects in our image, so for simplicity's sake we include the outline in the given test. If a real image/coordinates were given as our task, identifying the outline would be easy anyways.

\begin{code}
-- Restrict the domains of the outline. A line with arrow leaving a vertex will get Val 3, an incoming line gets Val 2.
setOutlineArrows :: [Domain] -> [(JunctionID,LineID,LineID,JunctionID)] -> Outline -> [Domain]
setOutlineArrows doms _ []              = doms
setOutlineArrows doms lineInfo ((x,y):xs) 
                  -- the following is safe if our Outline consists of connected junctions on the object, since:
                  -- if x<y we will have (x,i,i-1,y) in our lineInfo, otherwise we will have (y,i,i-1,x) as an element.
                  | getVar x < getVar y = let Just (_,i,_,_) = find (\line -> myfst line == x && myfrth line == y) lineInfo in 
                        (i,[Val 3]):(Var (getVar i-1),[Val 2]):
                          delete (i,[Val 0, Val 1, Val 2, Val 3]) (delete (Var (getVar i-1),[Val 0, Val 1, Val 2, Val 3]) (setOutlineArrows doms lineInfo xs))
                  | otherwise           = let Just (_,i,_,_) = find (\line -> myfst line == y && myfrth line == x) lineInfo in 
                        (i,[Val 2]):(Var (getVar i-1),[Val 3]):
                          delete (i,[Val 0, Val 1, Val 2, Val 3]) (delete (Var (getVar i-1),[Val 0, Val 1, Val 2, Val 3]) (setOutlineArrows doms lineInfo xs))

-- Increase readability of output of ac3. We remove the (empty) queue and show the Int instead of Var Int.
ac3onObject :: (Object, Outline) -> (Bool,[(Int, [Value])],[((Int,Int),[(Value,Value)])])
ac3onObject (object, outline) = (bool, map (\(var,vals) -> (getVar var, vals)) doms, map (\((x,y),pairs) -> ((getVar x, getVar y),pairs)) cons) where 
  CSP initdoms initcons = cspGenerator object
  (CSP doms cons, bool, _) = ac3 (CSP (setOutlineArrows initdoms (lineGeneration object (-1)) outline) initcons, True, initcons) 
\end{code}
