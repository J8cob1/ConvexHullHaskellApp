{-
- File: Algorithms.hs
- Date: 3/20/2020
- Description: contains the Convex Hull Algorithms used in this project
- Problem: these algorithms don't work right now
-}

{- Some references used:
-  [CLRS] - Cormon et al. "Introduction to Algorithms. Third Edition"
-  https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html
-  https://mathworld.wolfram.com/PolarAngle.html
-  https://stackoverflow.com/questions/2339487/calculate-angle-of-2-points/2339510 ?
-  https://stackoverflow.com/questions/8791768/haskell-selecting-lists-from-a-list-when-a-condition-is-true ?
-  https://math.stackexchange.com/questions/707673/find-angle-in-degrees-from-one-point-to-another-in-2d-space ?
-- CLRS Texbook
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html
-- https://mathworld.wolfram.com/PolarAngle.html
-- https://stackoverflow.com/questions/2339487/calculate-angle-of-2-points/2339510 ?
-- https://stackoverflow.com/questions/8791768/haskell-selecting-lists-from-a-list-when-a-condition-is-true ?
-- https://math.stackexchange.com/questions/707673/find-angle-in-degrees-from-one-point-to-another-in-2d-space ?
-- https://docs.python.org/2/library/cmath.html ?
-- https://hackage.haskell.org/package/cmath ?
-- https://www.mathsisfun.com/polar-cartesian-coordinates.html 
-- https://www.mathsisfun.com/geometry/radians.html
-}

module Algorithms where

-- Imports
import Data.List

-- Typedefs
type Number = Double
type Point2D = (Number, Number)



------------------------------------------------
-- Functions Common to one or more algorithms -- 
------------------------------------------------

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




-----------------------------------------------------------------------------------------------------------------------------------
-- Jarvis March - And functions specific to implementation                                                                       --
-- Jarvis March is a simple convex hull algorithm where we "gift warp"? our way around the set of points to find the convex hull --
-- My implementation attempts to simulate the working of the algorithm with list operations                                      --
-- Reference: "Introduction to Algorithms. Third Edition" [CLRS]                                                                 --
-----------------------------------------------------------------------------------------------------------------------------------

-- Calculate polar angle form starting point for every point in coords (with a map), then get the max of that..?
jarvisMarchConstructChain :: Bool -> [Point2D] -> [Point2D] -> Point2D -> [Point2D]
jarvisMarchConstructChain isRightChain (x:xs) [] endPoint = (x:xs)
jarvisMarchConstructChain isRightChain (x:xs) (y:ys) endPoint =
    if (nextHullPoint == endPoint) then
        (x:xs)
    else 
        jarvisMarchConstructChain isRightChain (nextHullPoint:x:xs) (drop 1 (y:ys)) endPoint
    where
        nextHullPoint = head (Data.List.sortBy (polarAngle isRightChain x) (removeFromList x (y:ys)))

-- Jarvis March Function
jarvisMarch :: [Point2D] -> [Point2D]
jarvisMarch points =
    if not (enoughPoints points) then
        []
    else
        [minPoint] ++ (jarvisMarchConstructChain False [minPoint] points maxPoint)
         ++
        [maxPoint] ++ (jarvisMarchConstructChain True [maxPoint] points minPoint)
        where
            minPoint = Data.List.minimum points
            maxPoint = Data.List.maximum points
            -- (removeFromList maxPoint points)

--------------------------------------------------------------------------------------------------------------------------------------------------
-- Graham's Scan - And functions specific to it's implementation                                                                                --
--   * Graham's Scan convex hull algorithm that uses a stack and a push/pop strategy to iteratively calculate the convex hull, in O(nlogn) time --
--   * Algorithm from: Cormon et al. "Introduction to Algorithms. Third Edition" [CLRS]                                                         --
-- My implementation attempts to simulate the working of the algorithm with list operations                                                     --
-------------------------------------------------------------------------------------------------------------------------------------------------- 

-- Remove all points in the list with the same polar angle (with respect to a given starting point)
-- Input:
--   * param1 {Point2D} (start): the starting point from which we base our polar angle calculations,
--   * param2 {[Point2D]} ((x:x1:...:xs)): the list we rare removing polar angle duplicates from
-- Output: true if function returned the expected result, False otherwise
-- Presumes the items are sorted by polar angle already
removePolarAngleDupsFromList :: Point2D -> [Point2D] -> [Point2D]
removePolarAngleDupsFromList start [] = []
removePolarAngleDupsFromList start (x:[]) = [x]
removePolarAngleDupsFromList (sx, sy) ((x1,y1):(x2,y2):xs) = 
    if ((x1,y1) /= (sx,sy) && (polarAngle False (sx, sy) (x1,y1) (x2,y2)) == EQ) then
        if (((x1 - sx)^2 + (y1 - sy)^2) > ((x2 - sx)^2 + (y2 - sy)^2)) then 
            (removePolarAngleDupsFromList (sx, sy) ((x1,y1):xs))
        else
            (removePolarAngleDupsFromList (sx, sy) ((x2,y2):xs))
    else
        [(x1,y1)] ++ (removePolarAngleDupsFromList (sx, sy) ((x2,y2):xs))

-- Compares the polar angles of two points with respect to a starting point and the positive x-axis, and returns an ordering specifying the result
-- This is meant to be a list sorting function
-- Input:
--   * param1 {Bool} negateiveXAxis: a boolean indicating whether to use the negative x axis.
--       This is not being used at the moment (it was, but I changed my mind), but I left it in here to be safe
--   * param2 {Point2D} (sx, sy): the starting point to which we base our polar angle calculations
--   * param3 {Point2D} (x1, y1): the first point whose polar angle we will calculate and compare
--   * param4 {Point2D} (x2, y2): the second point whose polar angle we will calculate and compare
-- Output:
--   * EQ if (x1, y1) and (x2, y2) have equal polar angles with respect to the starting point and positive x axis
--   * GT if (x1, y1) has a greater polar angle with respect to the starting point and positive x axis than (x2, y2)
--   * LT if (x1, y1) has a smaller polar angle with respect to the starting point and positive x axis than (x2, y2)
-- Idea: might want to implementing as polarAngle, polarAngleSort, polarAngleDupRemove, in case these end up getting used later
-- https://stackoverflow.com/questions/2339487/calculate-angle-of-2-points/2339510
-- https://docs.python.org/2/library/cmath.html
-- https://hackage.haskell.org/package/cmath
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
            if (y1 == sy) then 
                if (x1 == x1 || ((not negativeXAxis) && x1 > sx) || (negativeXAxis && x1 < sx)) then
                    0
                else
                    pi
            else if (x1 == sx) then
                if (y2 > sy) then
                    pi
                else
                    (3 * pi) / 2
            else if (x1 > sx && (not negativeXAxis)) then 
                atan ((y1 - sy) / (x1 - sx))
            else 
                3.14 - atan ((y1 - sy) / (x1 - sx))
        angle2 = 
            if (y2 == sy) then
                if (x2 == x2 || ((not negativeXAxis) && x2 > sx) || (negativeXAxis && x2 < sx)) then
                    0
                else
                    pi
            else if (x2 == sx) then
                if (y2 > sy) then
                    pi
                else
                    (3 * pi) / 2
            else if (x2 > sx && (not negativeXAxis)) then 
                atan ((y2 - sy) / (x2 - sx))
            else 
                3.14 - atan ((y2 - sy) / (x2 - sx))

-- Tells you if the lines formed by (sx, sy) and (x1, y1) and (x1, y1) and (x2, y2) make a "left turn". This is used by the Graham's Scan Algorithm
-- Input:
--   * param1 {Point2D} (sx, sy): the starting point from which we base our "left turn" calculation of off
--   * param2 {Point2D} (x1, y1): the first point from which we base our "left turn" calculation of off
--   * param3 {Point2D} (y2, y2): the second point from which we base our "left turn" calculation of off
-- Output: true if points form a left turn. False otherwise
-- [CLRS] "Introduction to Algorithms" Textbook
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

-- A part of the Graham's Scan algorithm that pops points off of the "stack" (list) if the two points and the given new point don't make a left turn
-- Once it sees that you've made a right turn, it puts the new point onto the stack
-- Input:
--   * param1 {Point2D} (newPoint): the starting point from which we base our "left turn" calculation of off
--   * param2 {[Point2D]} (p1:p2:ps): the "stack" we will pop points off of for our left turn calculation
-- Output: a set of points that, at the end of it's operation, will hopefully be the convex hull of a set of points
-- [CLRS] "Introduction to Algorithms" Textbook
gramsScanAmmendStack :: Point2D -> [Point2D] -> [Point2D]
gramsScanAmmendStack newPoint (p1:p2:ps) = 
    if not (makesLeftTurn newPoint p1 p2) then
        gramsScanAmmendStack newPoint (p2:ps)
    else
        newPoint:p1:p2:ps
gramsScanAmmendStack newPoint (x:xs) = (x:xs)

-- A part of the Graham's Scan algorithm that wraps and recursively calls the gramsScanAmmendStack function
-- The main gramsScan algorithm uses this
-- Once it sees that you've made a right turn, it puts the new point onto the stack
-- Input:
--   * param1 {[Point2D]} (x:xs): a list of points representing a stack. 
--   * param2 {[Point2D]} (y:ys): 
-- Output: a set of points that, at the end of it's operation, will hopefully be the convex hull of a set of points
-- [CLRS] "Introduction to Algorithms" Textbook
gramScanAddNextPoint :: [Point2D] -> [Point2D] -> [Point2D]
gramScanAddNextPoint [] _ = []
gramScanAddNextPoint (x:xs) [] = (x:xs)
gramScanAddNextPoint (x:xs) (y:ys) = 
    gramScanAddNextPoint cleanedList ys
    where
        cleanedList = gramsScanAmmendStack y (x:xs)

-- The main function of the Graham's Scan algorithm
-- Input {[Point2D]}: a list of points you want to find the conved hull of
-- Output: ideally, the convex hull of the set of points, if it has one
-- [CLRS] "Introduction to Algorithms" Textbook
grahamsScan :: [Point2D] -> [Point2D]
grahamsScan points = 
    if not (enoughPoints sortedPoints) then
        []
    else
        (gramScanAddNextPoint (reverse (take 3 sortedPoints)) ((drop 3 sortedPoints)))
    where
        minPoint = Data.List.minimum points
        sortedPoints = removePolarAngleDupsFromList minPoint (Data.List.sortBy (polarAngle False minPoint) points)