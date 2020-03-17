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

{- Functions Common to one or more algorithms -} 
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

{-
- Jarvis March - A simple convex hull algorithm where we "gift warp"? our way around the set of points to find the convex hull
- Reference: "Introduction to Algorithms. Third Edition" [CLRS]
- TODO: test
- TODO: performance testing
-}
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

{-
- Graham's Scan - A simple convex hull algorithm where we "gift warp"? our way around the set of points to find the convex hull
- Algorithm from: Cormon et al. "Introduction to Algorithms. Third Edition" [CLRS]
- TODO: performance testing
-}
-- Remove all points in the list with the same polar angle (with respect to a given starting point)
-- Presumes the items are ALREADY sorted by polar angle
removeDupsFromList :: Point2D -> [Point2D] -> [Point2D]
removeDupsFromList start [] = []
removeDupsFromList start (x:[]) = [x]
removeDupsFromList (sx, sy) ((x1,y1):(x2,y2):xs) = 
    if ((x1,y1) /= (sx,sy) && (polarAngle False (sx, sy) (x1,y1) (x2,y2)) == EQ) then
        if (((x1 - sx)^2 + (y1 - sy)^2) > ((x2 - sx)^2 + (y2 - sy)^2)) then 
            (removeDupsFromList (sx, sy) ((x1,y1):xs))
        else
            (removeDupsFromList (sx, sy) ((x2,y2):xs))
    else
        [(x1,y1)] ++ (removeDupsFromList (sx, sy) ((x2,y2):xs))

-- Calculates the angle between two points with respect to the positive x-axis
-- INPUT:
--     axis: a boolean indicating whether to use the positive x axis
--     first: a point indicating if the starting point
--     second: a
-- might want to implement as polarAngle, polarAngleSort, polarAngleDupRemove?
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
gramScanCleanUpList :: Point2D -> [Point2D] -> [Point2D]
gramScanCleanUpList newPoint (p1:p2:ps) = 
    if not (makesLeftTurn newPoint p1 p2) then
        gramScanCleanUpList newPoint (p2:ps)
    else
        newPoint:p1:p2:ps
gramScanCleanUpList newPoint (x:xs) = (x:xs)

gramScanAddNextPoint :: [Point2D] -> [Point2D] -> [Point2D]
gramScanAddNextPoint [] _ = []
gramScanAddNextPoint (x:xs) [] = (x:xs)
gramScanAddNextPoint (x:xs) (y:ys) = 
    gramScanAddNextPoint cleanedList ys
    where
        cleanedList = gramScanCleanUpList y (x:xs)

grahamScan :: [Point2D] -> [Point2D]
grahamScan points = 
    if not (enoughPoints sortedPoints) then
        []
    else
        (gramScanAddNextPoint (reverse (take 3 sortedPoints)) ((drop 3 sortedPoints)))
    where
        minPoint = Data.List.minimum points
        sortedPoints = removeDupsFromList minPoint (Data.List.sortBy (polarAngle False minPoint) points)