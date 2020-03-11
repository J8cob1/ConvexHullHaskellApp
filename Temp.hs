-- Convex Hull Algorithms take a set of points in some dimension and return a set such that the hull around the such that the rest of the points in the input set are contained within it 
-- We will be working with several algorithms, including:
--  - Optimal Algorithms
--    * Chan's Algorithm
--    * Divide and COnquer
--  - Graham's Scan
--  - Jarvis March
--  etc
-- 

-- Info on Data.lists and data.ord from https://hackage.haskell.org/ used

import Data

-- For easier referencing
type Point2D = (Int, Int)

-- Find the point in the list with the lowest x coordinate. And if collision, lowest y coordinate
lowestPoint :: [Point2D] -> Point2D
lowestPoint listOfPoints = Data.minimum (listOfPoints)

-- Non-Optimal Algorithms
-- Algorithm:
-- 1. Pick a starting point on the edge of the hull
-- 2. Find the next edge of the hull, by drawing a line from the starting point to every other point to see which one forms a line that has no other points to the left of it. Add that point to the hull
-- 3. Repeat step 2, using the last point you added to the hull as the starting point. Continue until you reach the end of the hull
-- 4. Return the hull
-- data direction = Left | Right

-- Takes a starting point and a list of other coordinates and returns the next item on the edge of the convex hull
-- INPUT: 
--    start - the starting point from which calculations are performed
--    coords - a list of points from which to derive the next Vertex on the Hull
-- OUTPUT: the next vertex of the hull
-- Expected Performance: O(hn) where h is the number of vertices on the hull  -CLRS
jarvisMarchGetNextVertex :: Point2D -> [Point2D] -> Point2D
jarvisMarchGetNextVertex start coords = 

JarvisMarchHull :: [Point2D] -> [Point2D]
JarvisMarchHull coords = 
    map (\ -> ) coords 
    where 
        start = lowestPoint(coords)


GrahmsScanHull :: [Point2D] -> [Point2D]
GrahmsScanHull = undefined

ChansAlgorithm :: [Point2D] -> [Point2D]
ChansAlgorithm = undefined


--  Optimal Algorithms --
DivideAndConquerHull :: [Point2D] -> [Point2D]
DivideAndConquerHull = undefined

ChansAlgorithmHull :: [Point2D] -> [Point2D]
ChansAlgorithmHull = undefined