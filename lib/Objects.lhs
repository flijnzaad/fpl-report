\section{Objects}

\begin{code}
module Objects where

import CSP
import AC3
import Data.List
import Data.Char

-- Variables: Lines have negative IDs, junctions positive IDs
-- Values:    Lines: 0 = "-", 1 = "+", 2 = "incoming <", 3 = "outgoing >"
--         Junction: 0-5 = "L junction", 10-15 = "Fork", 20-23 = "T junction", 30-33 = "Arrow"

type LineID     = Int
type JunctionID = Int
data Junction   = L JunctionID JunctionID JunctionID 
                | Fork JunctionID JunctionID JunctionID JunctionID 
                | T JunctionID JunctionID JunctionID JunctionID 
                | Arrow JunctionID JunctionID JunctionID JunctionID
data Line       = Line JunctionID LineID LineID
type Object = [Junction] 

myfst, myfrth :: (JunctionID,LineID,LineID,JunctionID) -> JunctionID
mysnd, mytrd  :: (JunctionID,LineID,LineID,JunctionID) -> LineID

myfst  (a, _, _, _) = a
mysnd  (_, b, _, _) = b
mytrd  (_, _, c, _) = c
myfrth (_, _, _, d) = d

-- cycle through list of junctions, and add two lines between all connected junctions. 
lineGeneration :: Object -> LineID -> [(JunctionID,LineID,LineID,JunctionID)]
lineGeneration [] _                         = []
lineGeneration ((L i j id):xs) lineID       = lineCreation id [i,j] lineID   ++ lineGeneration xs (lineID-2*length (lineCreation id [i,j] lineID))
lineGeneration ((Fork i j k id):xs) lineID  = lineCreation id [i,j,k] lineID ++ lineGeneration xs (lineID-2*length (lineCreation id [i,j,k] lineID))
lineGeneration ((T i j k id):xs) lineID     = lineCreation id [i,j,k] lineID ++ lineGeneration xs (lineID-2*length (lineCreation id [i,j,k] lineID))
lineGeneration ((Arrow i j k id):xs) lineID = lineCreation id [i,j,k] lineID ++ lineGeneration xs (lineID-2*length (lineCreation id [i,j,k] lineID))

lineCreation :: JunctionID -> [JunctionID] -> LineID -> [(JunctionID,LineID,LineID,JunctionID)]
lineCreation _ [] _ = []
lineCreation id (i:is) lineID = if id < i then (id, lineID, lineID-1, i):lineCreation id is (lineID-2) else lineCreation id is (lineID-2)


junctionConstraints :: Object -> [(JunctionID,LineID,LineID,JunctionID)] -> ([Domain], [Constraint])
junctionConstraints [] _ = ([],[])
-- We consider the junctions one by one. We have already chosen the IDs of the lines between two junctions, and we use to the lineto lambda to find the correct ID
junctionConstraints ((L i j id):xs) lineInfo       = ((id,[0..5]):doms,[
  ((id,lineto i), [(0,2),(1,3),(2,3),(3,1),(4,2),(5,0)]),
  ((id,lineto j), [(0,3),(1,2),(2,1),(3,2),(4,0),(5,5)])
  ] ++ cons) where
  (doms, cons) = junctionConstraints xs lineInfo
  lineto = \n -> head (map mysnd (filter (\line -> myfst line == id && myfrth line == n) lineInfo) ++ map mytrd (filter (\line -> myfst line == n && myfrth line == id) lineInfo))
    -- Either (id, correctline, _ , i) or (i, _, correctline, id) appears in the output of lineGeneration, so head will always work.
junctionConstraints ((Fork i j k id):xs) lineInfo  = ((id,[10..12]):doms,[
  ((id,lineto i), [(10,1),(11,0),(12,0),(12,2),(12,3)]),
  ((id,lineto j), [(10,1),(11,0),(12,0),(12,2),(12,3)]),
  ((id,lineto k), [(10,1),(11,0),(12,0),(12,2),(12,3)])
  ] ++ cons) where
  (doms, cons) = junctionConstraints xs lineInfo
  lineto = \n -> head (map mysnd (filter (\line -> myfst line == id && myfrth line == n) lineInfo) ++ map mytrd (filter (\line -> myfst line == n && myfrth line == id) lineInfo))
junctionConstraints ((T i j k id):xs) lineInfo      = ((id,[20..23]):doms,[
  ((id,lineto i), [(20,2),(21,2),(22,3),(23,2)]),
  ((id,lineto j), [(20,3),(21,3),(22,3),(23,3)]),
  ((id,lineto k), [(20,2),(21,3),(22,1),(23,0)])
  ] ++ cons) where
  (doms, cons) = junctionConstraints xs lineInfo
  lineto = \n -> head (map mysnd (filter (\line -> myfst line == id && myfrth line == n) lineInfo) ++ map mytrd (filter (\line -> myfst line == n && myfrth line == id) lineInfo))
junctionConstraints ((Arrow i j k id):xs) lineInfo  = ((id,[30..32]):doms,[
  ((id,lineto i), [(30,3),(31,1),(32,0)]),
  ((id,lineto j), [(30,2),(31,1),(32,0)]),
  ((id,lineto k), [(30,1),(31,0),(32,1)])
  ] ++ cons) where
  (doms, cons) = junctionConstraints xs lineInfo
  lineto = \n -> head (map mysnd (filter (\line -> myfst line == id && myfrth line == n) lineInfo) ++ map mytrd (filter (\line -> myfst line == n && myfrth line == id) lineInfo))

-- The Object type is nice and readable, this function generates the correct csp. 
-- It preserves the JunctionIDs used in the Object def.
-- !! Junctions must have positive IDs

cspGenerator :: Object -> Problem
cspGenerator object = CSP ([0..length allJuncDomains -1]++[-2*length lineInfo..(-1)]) (allJuncDomains ++ allLineDomains) (allJuncConstraints ++ allLineConstraints) where
  lineInfo                              = lineGeneration object (-1) -- first line gets variable -1
  (allJuncDomains, allJuncConstraints)  = junctionConstraints object lineInfo
  allLineDomains      = concat [[(2*line,[0..3]),(2*line-1,[0..3])] | line <- [-length lineInfo..(-1)] ]
  allLineConstraints  = [ ((2*line, 2*line-1), [(0,0),(1,1),(2,3),(3,2)]) | line <- [-length lineInfo..(-1)] ]


-- Input the outline arrows as a first step. A line with arrow leaving a vertex will get Val 3, an incoming line gets Val 2
-- The third argument is the list of Junction pairs we identify as outline.
setOutlineArrows :: [Domain] -> [(JunctionID,LineID,LineID,JunctionID)] -> [(JunctionID,JunctionID)] -> [Domain]
setOutlineArrows doms _ []              = doms
setOutlineArrows doms lineInfo ((x,y):xs) 
                  -- the following is safe if our list of border junctions is actually a path on the object, since:
                  -- if x<y we will have (x,i,i-1,y) in our lineInfo, otherwise we will have (y,i,i-1,x) as an element.
                              | x<y       = let Just (_,i,_,_) = find (\line -> myfst line == x && myfrth line == y) lineInfo in 
                                  (i,[3]):(i-1,[2]):delete (i,[0..3]) (delete (i-1,[0..3]) (setOutlineArrows doms lineInfo xs))
                              | otherwise = let Just (_,i,_,_) = find (\line -> myfst line == y && myfrth line == x) lineInfo in 
                                  (i,[2]):(i-1,[3]):delete (i,[0..3]) (delete (i-1,[0..3]) (setOutlineArrows doms lineInfo xs))

cube :: Object
cube = [Arrow 1 5 6 0, L 0 2 1, Arrow 3 1 6 2, L 2 4 3, Arrow 5 3 6 4, L 4 0 5, Fork 0 2 4 6]

cubeOutline :: [(JunctionID,JunctionID)]
cubeOutline = [(0,1),(1,2),(2,3),(3,4),(4,5),(5,0)]

cubeCSPtest :: Problem 
cubeCSPtest = CSP vars (setOutlineArrows doms (lineGeneration cube (-1)) cubeOutline) cons
  where CSP vars doms cons = cspGenerator cube

ac3input = let CSP vars doms cons = cspGenerator cube in (cubeCSPtest, True, cons)

\end{code}