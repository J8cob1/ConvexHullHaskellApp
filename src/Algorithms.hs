{-
- File: Algorithms.hs
- Date: 3/20/2020
- Description: contains the Convex Hull Algorithms used in this project
- Problem: these algorithms don't work right now
-}

module Algorithms where

-- Imports
import Data.List

-- Typedefs
type Number = Double
type Point2D = (Number, Number)

-- To-Do: Clean out points that have the same polar angle. Pick the one farthest from the min point and filter (in here)
-- 

-- Sources/References:
-- CLRS Texbook
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html
-- https://mathworld.wolfram.com/PolarAngle.html
-- https://stackoverflow.com/questions/2339487/calculate-angle-of-2-points/2339510 ?
-- https://stackoverflow.com/questions/8791768/haskell-selecting-lists-from-a-list-when-a-condition-is-true ?
-- https://math.stackexchange.com/questions/707673/find-angle-in-degrees-from-one-point-to-another-in-2d-space ?
-- Functions Common to one or more algorithms 

-- Checks if there are at least three points in this list
-- Returns true if so, false otherwise
-- Simple enough that no test is needed
enoughPoints :: [Point2D] -> Bool
enoughPoints coords = (length coords) > 2

-- Removes a target item from a list and returns the resulting list
removeFromList :: Point2D -> [Point2D] -> [Point2D]
removeFromList item [] = []
removeFromList item (x:xs) = 
    if x == item then
        xs
    else
        [x] ++ (removeFromList item xs)

-- https://www.mathsisfun.com/polar-cartesian-coordinates.html 
-- https://www.mathsisfun.com/geometry/radians.html
polarAngle :: Bool -> Point2D -> Point2D -> Point2D -> Ordering
polarAngle negativeXAxis (sx, sy) (x1, y1) (x2, y2) =  
    if (angle1 > angle2) then
        GT
    else if (angle1 < angle2) then
        LT
    else
        EQ
    where
        angle1 = 
            if (x1 == sx) then
                0
            else if (x1 > sx) then
                atan ((y1 - sy) / (x1 - sx))
            else
                3.14 - atan ((y1 - sy) / (x1 - sx))
        angle2 = 
            if (x2 == sx) then
                0
            else if (x2 > sx) then
                atan ((y2 - sy) / (x2 - sx))
            else
                3.14 - atan ((y2 - sy) / (x2 - sx))

--                angle1 = 
--            if (negativeXAxis) then
--                atan ((y1 - sy) / (sx - x1)) -- not sure if this is right
--            else
--                atan ((y1 - sy) / (x1 - sx))
--        angle2 = atan ((y2 - sy) / (x2 - sx))

{-
- Jarvis March - A simple convex hull algorithm where we "gift warp"? our way around the set of points to find the convex hull
- Algorithm:
- 1. Pick a starting point on the edge of the hull
- 2. Find the next edge of the hull, by drawing a line from the starting point to every other point to see which one forms a line that has no other points to the left of it. Add that point to the hull
- 3. Repeat step 2, using the last point you added to the hull as the starting point. Continue until you reach the end of the hull
- 4. Return the hull
- INPUT: 
-     start - the starting point from which calculations are performed
-     coords - a list of points from which to derive the next Vertex on the Hull
- OUTPUT: the next vertex of the hull
- PRESUMPTIONS: no two points share the same x and y coords
- TODO: test
- TODO: performance testing
-}

-- CLRS
jarvisMarchConstructChain :: Bool -> [Point2D] -> [Point2D] -> Point2D -> [Point2D]
jarvisMarchConstructChain isRightChain (x:xs) [] endPoint = (x:xs)
jarvisMarchConstructChain isRightChain (x:xs) (y:ys) endPoint =
    if (nextHullPoint == endPoint) then
        (x:xs)
    else 
        jarvisMarchConstructChain isRightChain (nextHullPoint:x:xs) (drop 1 (y:ys)) endPoint
    where
        nextHullPoint = head (Data.List.sortBy (polarAngle isRightChain x) (removeFromList x (y:ys)))


jarvisMarch :: [Point2D] -> [Point2D]
jarvisMarch points =
    if not (enoughPoints points) then
        []
    else
        (jarvisMarchConstructChain False [minPoint] (removeFromList minPoint points) maxPoint)
        ++
        (jarvisMarchConstructChain True [maxPoint] (removeFromList maxPoint points) minPoint)
        where
            minPoint = Data.List.minimum points
            maxPoint = Data.List.maximum points


{- Graham's Scan -}
-- CLRS. "Introduction to Algorithms" Textbook
makesLeftTurn :: Point2D -> Point2D -> Point2D -> Bool
makesLeftTurn (sx, sy) (x1, y1) (x2, y2) =  
    if (crossProduct < 0) then
        True
    else
        False
    where
        x1m0 = (x1 - sx)
        y1m0 = (y1 - sy)
        x2m0 = (x2 - sx)
        y2m0 = (y2 - sy)
        crossProduct = x1m0 * y2m0 - x2m0 * y1m0 -- (p2 - p0) x (p1 - p0)

-- Utter nonsense
-- gramScanAddNextPoint
gramScanCleanUpList :: {-Point2D -> [Point2D] -> -} Point2D -> [Point2D] -> [Point2D]
-- gramScanCleanUpList newPoint [] = []
gramScanCleanUpList newPoint (p1:p2:ps) = 
    if not (makesLeftTurn newPoint p1 p2) then
        gramScanCleanUpList newPoint (p2:ps)
    else
        newPoint:p1:p2:ps
    --else
    --    newPoint:p1:p2:p3:ps
--gramScanCleanUpList newPoint (x:xs) = (x:xs)

gramScanAddNextPoint :: [Point2D] -> [Point2D] -> [Point2D]
gramScanAddNextPoint [] _ = []
gramScanAddNextPoint (x:xs) [] = (x:xs)
gramScanAddNextPoint (x:xs) (y:ys) = 
    gramScanAddNextPoint cleanedList ys
    where
        cleanedList = gramScanCleanUpList y (x:xs)

grahamScan :: [Point2D] -> [Point2D]
grahamScan points = 
    if not (enoughPoints points) then
        []
    else
        (gramScanAddNextPoint (take 3 sortedPoints) ((drop 3 sortedPoints) ++ [minPoint]))
        where
            minPoint = Data.List.minimum points 
            sortedPoints = Data.List.sortBy (polarAngle False minPoint) points
            -- (removeFromList minPoint points)

-- Finds the point in the list with the lowest x and y coordinate
-- TODO: create QuickCheck test. Otherwise, it seems like it works

-- Calculates the angle between two points, with respect to either the negative or positive x axis
-- INPUT:
--     axis: a boolean indicating whether to use the positive x axis
--     first: a point indicating if the starting point
--     second: a
-- https://stackoverflow.com/questions/2339487/calculate-angle-of-2-points/2339510
-- https://docs.python.org/2/library/cmath.html
-- https://hackage.haskell.org/package/cmath
{-angle :: Bool -> Point2D -> Point2D -> (Number, Point2D)
angle axis (x1, y1) (x2, y2) =
    if (x1 == x2 && y1 == y2) then 
        (20, (x2, y2))
    else 
        case axis of
            True -> (atan2 (y2 - y1) (x2 - x1), (x2, y2))
            False -> (atan2 (y1 - y2) (x2 - x1), (x2, y2))-}


--jarvisMarchHull [(0.0, 0.0), (1.0, 1.0), (2.0, 0.0), (2.0, 3.0), (0.0, 3.0), (1.0, 2.0)]
-- Calculate polar angle form starting point for every point in coords (with a map), then get the max of that 
-- [(0.0, 0.0), (1.1, 1.1), (2.2,2.2)]
