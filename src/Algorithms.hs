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

-- Typedefs and custom types
type Number = Double
type Point2D = (Number, Number)
type Line = (Point2D, Point2D)
data PointPosition = Left | Right | OnLine | Bottom | Top deriving (Eq)-- this is used for multiple purposes. https://stackoverflow.com/questions/28396572/how-i-can-compare-a-variable-with-a-data-type-in-haskell 

-- Functions --

-- Takes a tuple of two points (that form a line), and a third point and determines whether the point is on the left or right side of the line
-- Input:
--   * ((l1x,l1y),(l2x,l2y)) - the two points that form the line
--   * (px, py) - the point outside the line
-- Output: Left if the point is on the left of the line, or Right if it is on the right
-- [CLRS] "Introduction to Algorithms" Textbook
-- https://www.youtube.com/watch?v=Vu84lmMzP2o
sideOfLine :: Line -> Point2D -> PointPosition
sideOfLine ((l1x,l1y),(l2x,l2y)) (px, py) =
    -- Do a cross product
    if (crossProduct > 0) then
        Algorithms.Left
    else if (crossProduct < 0) then
        Algorithms.Right
    else
        Algorithms.OnLine
    where
        p2psX = px - l1x
        p2psY = py - l1y
        p1psX = l2x - l1x
        p1psY = l2y - l1y
        crossProduct = (p2psX * p1psY) - (p1psX * p2psY) -- (p2 - p0) x (p1 - p0)

-- Similar to side of line, except that it takes a point position as an argument and returns true if the point is ineed
-- onSideOfLine :: Line -> Point2D -> PointPosition -> Bool
-- onSideOfLine ((l1x,l1y),(l2x,l2y)) (px, py) side =
--     -- Do a cross product
--     if (crossProduct > 0) then
--         side == Left
--     else if (crossProduct < 0)
--         side == Right
--     else
--         side == OnLine
--     where
--         p2psX = px - l1x
--         p2psY = py - l1y
--         p1psX = l2x - l1x
--         p1psY = l2y - l1y
--         crossProduct = (p2psX * p1psY) - (p1psX * p2psY) -- (p2 - p0) x (p1 - p0)

-- Takes a list of points, a line, and a point psoition and tells you if any of them points in the point list are on the "point position" side of the list
-- Input:
-- Output:
--anyPointsOnSideOfLine :: [Point2D] -> Line -> PointPosition -> Bool 
--anyPointsOnSideOfLine listOfPoints line side =
--    any (\point -> (sideOfLine line point) == side) listOfPoints

-- Go through a list of points and find the point whose line is on the left side
--search :: Line -> [Point2D] -> [Point2D] -> Point2D
--search _ [] _ = True
--search _ _ [] = True
--search line points (x:xs) = 
--    any (\x -> (sideOfLine line x) == False) listOfPoints
    -- Left of point
--    if (sideOfLine line x) then
--        if leftside of x is left
--    else
--        sideOfLine

-- Jarvis March Algorithm
-- Get point on hull
-- Get rightmost/leftmost point after that
-- Keep going until you build the hull
--jarvisMarch :: [Point2D] -> [Point2D]
--jarvisMarch [] = []
--jarvisMarch (x,y,[]) = []
--jarvisMarch (x:y:z:xs) = 
--    jarvisMarch = dsfdfd
--    where
--        nextPoint

--jarvisMarchGetNext :: [Point2D] -> [Point2D] -> [Point2D]
--jarvisMarchGetNext known_points [] = []
--jarvisMarchGetNext known_points (x,y,[]) = []
--jarvisMarchGetNext known_points (x:y:z:xs) = 
--    -- First case is different if there are less than two points in the convex hull
--    if length known_points < 2 then
--        for all points in x1
--            if any (\x -> (sideOfLine line x) == Right) listOfPoints then return false
--    else
--        sdf
--    where
--        starting_point = head known_points
--        checkNextPoint = any (\x -> (sideOfLine line x) == Right) listOfPoints

    

------------------------------------------------
-- Functions Common to one or more algorithms -- 
------------------------------------------------

-- A data.List sorting function to help us find the minimum of a set of points by y coordinate first, then by x
-- INPUT:
--  * param1 {Point2D}: the first point you want to compare
--  * param2 {Point2D}: the second point you want to compare
-- OUTPUT: an ordering of the two points
farthest :: PointPosition -> Point2D -> Point2D -> Ordering
farthest position (x1, y1) (x2, y2) =
    case position of
        Algorithms.Left -> -- Sorts results in: most left on right, Least left on left
            compare (x2, y2) (x1, y1) 
        Algorithms.Right ->  -- Sorts results in: most right on right, Least right on left
            compare (x1, y1) (x2, y2)
        Algorithms.Top -> -- Sorts results in: most top on right, Least top on left
            if (y1 < y2) then LT
            else if (y1 > y2) then GT
            else
                if (x1 < x2) then LT
                else if (x1 > x2) then GT
                else EQ
        _ -> -- Default for "OnLine" and "Bottom" is bottom
            if (y1 < y2) then GT
            else if (y1 > y2) then LT
            else
                if (x1 < x2) then LT
                else if (x1 > x2) then GT
                else EQ

-- A wrapper function to get the lowest point of a list of points (lowest as in, point with the least y-coordinate is lowest)
-- Helpful to avoid too much extra code
-- INPUT {[Point2D]}: the list you want to get the lowest item from
-- OUTPUT: the lowest item in the list
getLowestPoint :: [Point2D] -> Point2D
getLowestPoint listOfPoints = Data.List.minimumBy (farthest Top) listOfPoints

-- A wrapper function to get the highest point of a list of points (highest as in, point with the greatest y-coordinate is highest)
-- Helpful to avoid too much extra code
-- INPUT {[Point2D]}: the list you want to get the highest item from
-- OUTPUT: the highest item in the list
getHighestPoint :: [Point2D] -> Point2D
getHighestPoint listOfPoints = Data.List.maximumBy (farthest Top) listOfPoints

-- Checks if there are at least three points in this list
-- Returns true if so, false otherwise
-- Simple enough that no test is needed
-- INPUT {[Point2D]}: the list you want to check the size of
-- OUTPUT: true if there at least three items. False if not
enoughPoints :: [Point2D] -> Bool
enoughPoints coords = (length coords) > 2

-- Removes a target point from a list of points and returns the resulting list
-- INPUT:
--  *param1 {Point2D}: the point you want to remove
--  *param2 {[Point2D]}: the list youwa to remove the specific point from
-- OUTPUT: the lowest item in the list
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

-- Gets the next point in the convex hull given the lowest point on the hull, a list of points already on the hull, and a list of points not in the hull
-- broken, I think
-- INPUT:
--  *param1 {Point2D} (start): the starting point of the convex hull claculation. More specifically, the lowest (in terms of y [and x, if needed] coordinate)
--  *param2 {[Point2D]} (pointsOnHull): the points already in the convex hull
--  *param3 {[Point2D]} (restOfPoints): the points not on the convex hull (with the exception of the starting point, which should be here so the algorithm knows when to halt)
-- OUTPUT: the convex hull calculated using the information in the parameters after recursion is said and done, hopefully
jarvisMarchAddNextPoint :: Point2D -> [Point2D] -> [Point2D] -> [Point2D]
jarvisMarchAddNextPoint start [] [] = []
jarvisMarchAddNextPoint start pointsOnHull [] = pointsOnHull
jarvisMarchAddNextPoint start pointsOnHull restOfPoints =
    -- Stops the recursion
    if (nextHullPoint == start) then
        pointsOnHull
    else
        jarvisMarchAddNextPoint start (nextHullPoint:pointsOnHull) (removeFromList nextHullPoint restOfPoints)
    where
        lastHullPoint = head pointsOnHull
        workingSet = 
            if (lastHullPoint == start) then 
                removeFromList start restOfPoints
            else
                restOfPoints
        nextHullPoint = 
            head (reverse (removePolarAngleDupsFromList lastHullPoint (Data.List.sortBy (polarAngle False lastHullPoint) workingSet)))

-- The main function of the Jarvis-March algorithm
-- Input {[Point2D]}: a list of points you want to find the conved hull of
-- Output: ideally, the convex hull of the set of points, if it has one
-- [CLRS] "Introduction to Algorithms" Textbook - though I changed my implementation to something else because I thought it would work (so the implementation is not exactly from here anymore)
jarvisMarch :: [Point2D] -> [Point2D]
jarvisMarch points =
    if (not (enoughPoints points)) || (not (enoughPoints convexHull)) then
        []
    else
        convexHull
    where
        minPoint = getLowestPoint points
        processedPoints = removePolarAngleDupsFromList minPoint (Data.List.sortBy (polarAngle False minPoint) points)
        convexHull = jarvisMarchAddNextPoint minPoint [minPoint] processedPoints

-- ALgorithm: 
-- 1. start with the leftmost point
-- 2. 

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

-- Compares the polar angles of two points with respect to a starting point and the x-axis, and returns an ordering specifying the result
-- This is meant to be a list sorting function
-- Input:
--   * param1 {Bool} negateiveXAxis: a boolean indicating whether to use the negative x axis.
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
    compare angle1 angle2
    where
        angle1 = atan2 (y1 - sy) (x1 - sx)
        angle2 = atan2 (y2 - sy) (x2 - sx)

-- Tells you if the lines formed by (sx, sy) and (x1, y1) and (x1, y1) and (x2, y2) make a "left turn". This is used by the Graham's Scan Algorithm
-- Input:
--   * param1 {Point2D} (sx, sy): the starting point from which we base our "left turn" calculation of off
--   * param2 {Point2D} (x1, y1): the first point from which we form the first and second lines in our calculation
--   * param3 {Point2D} (y2, y2): the second point from which we form the second and third lines of our calculation
-- Output: true if points form a left turn. False otherwise
-- [CLRS] "Introduction to Algorithms" Textbook
makesLeftTurn :: Point2D -> Point2D -> Point2D -> Bool
makesLeftTurn (sx, sy) (x1, y1) (x2, y2) =  
    if (crossProduct < 0) then
        True
    else
        False
    where
        p2psX = x2 - sx
        p2psY = y2 - sy
        p1psX = x1 - sx
        p1psY = y1 - sy
        crossProduct = (p2psX * p1psY) - (p1psX * p2psY) -- (p2 - p0) x (p1 - p0)

-- A part of the Graham's Scan algorithm that pops points off of the "stack" (list) if the two points and the given new point don't make a left turn
-- Once it sees that you've made a right turn, it puts the new point onto the stack
-- Input:
--   * param1 {Point2D} (newPoint): the starting point from which we base our "left turn" calculation of off
--   * param2 {[Point2D]} (top:nextToTop:rest): the "stack" we will pop points off of for our left turn calculation
-- Output: a set of points that, at the end of it's operation, will hopefully be the convex hull of a set of points
-- [CLRS] "Introduction to Algorithms" Textbook
gramsScanAmmendStack :: Point2D -> [Point2D] -> [Point2D]
gramsScanAmmendStack newPoint [] = [newPoint]
gramsScanAmmendStack newPoint (top:[]) = newPoint:top:[]
gramsScanAmmendStack newPoint (top:nextToTop:[]) = newPoint:top:nextToTop:[]
gramsScanAmmendStack newPoint (top:nextToTop:rest) = 
    if not (makesLeftTurn nextToTop top newPoint) then
        gramsScanAmmendStack newPoint (nextToTop:rest)
    else
        newPoint:top:nextToTop:rest

-- A part of the Graham's Scan algorithm that wraps and recursively calls the gramsScanAmmendStack function. 
-- It basically helps add/remove items to the list representing the sorted list of points as we process them.
-- Input:
--   * param1 {[Point2D]} (x:xs): a list of points representing a stack. 
--   * param2 {[Point2D]} (y:ys): the list of points we have left to process
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
    if not (enoughPoints points) || not (enoughPoints convexHull) then
        []
    else
        convexHull
    where
        minPoint = getLowestPoint points
        sortedPoints = removePolarAngleDupsFromList minPoint (Data.List.sortBy (polarAngle False minPoint) points)
        convexHull = gramScanAddNextPoint (reverse (take 3 sortedPoints)) ((drop 3 sortedPoints))





--------------------------------------------------------------------------------------------------------------------------------------------------
-- Quick Hull - And functions specific to it's implementation                                                                                   --
--  * Algorithm from Wikipedia: https://en.wikipedia.org/wiki/Quickhull                                                                         --
-------------------------------------------------------------------------------------------------------------------------------------------------- 

-- A function that takes three points and returns something. We will use it to decide if a point is inside a triangle or not
-- Copied/transcribed from here: https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
sign :: Point2D -> Point2D -> Point2D -> Number
sign (x1, y1) (x2, y2) (x3, y3) = (x1 - x3) * (y2 - y3) - (x2 - x3) * (y1 - y3)

-- Determines if a point is inside a triangle or not
-- Copied/transcribed from here: https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
-- Input:
--  * trianglePoint1, trianglePoint2, trianglePoint3 - the points representing the triangle
--  * nonTrianglePoint - the point we want to know about (is it outside or inside the traingle?)
-- Output:
pointInsideTriangle :: Point2D -> Point2D -> Point2D -> Point2D -> Bool
pointInsideTriangle trianglePoint1 trianglePoint2 trianglePoint3 nonTrianglePoint =
    not (hasNegative && hasPositive) -- https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:not
    where
        d1 = sign nonTrianglePoint trianglePoint1 trianglePoint2
        d2 = sign nonTrianglePoint trianglePoint2 trianglePoint3
        d3 = sign nonTrianglePoint trianglePoint3 trianglePoint1
        hasNegative = (d1 < 0) || (d2 < 0) || (d3 < 0)
        hasPositive = (d1 > 0) || (d2 > 0) || (d3 > 0)

-- "Find points on convex hull from the set Sk of points that are on the right side of the oriented line from P to Q" -https://en.wikipedia.org/wiki/Quickhull
-- A quickhull helper function
-- Input: 
--  * inputList - the list of points we are finding the convex hull from
--  * ((px, py),(qx, qy)) - the line
-- Output: 
-- https://en.wikipedia.org/wiki/Quickhull
findHull :: [Point2D] -> Line -> [Point2D]
findHull [] _ = []
findHull inputList ((px, py),(qx, qy)) = 
    convexHull ++ (findHull subset1 ((px, py), farthestPoint)) ++ (findHull subset2 (farthestPoint, (qx, qy))) 
    where
        -- From the given set of points in Sk, find farthest point, say C, from segment PQ -https://en.wikipedia.org/wiki/Quickhull
        -- https://www.geeksforgeeks.org/quickhull-algorithm-convex-hull/ and https://stackoverflow.com/questions/45917027/haskell-max-number-in-a-list
        -- https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:abs
        farthestPoint = foldr1 (\(xx,xy) (yx, yy) ->
                                    let x = abs((xy - py) * (qx - px) - (qy - py) * (xx - px)) in
                                    let y = abs((yy - py) * (qx - px) - (qy - py) * (yx - px)) in
                                    if x >= y then (xx,xy) else (yx, yy)
                                ) inputList

        -- Add point C to convex hull at the location between P and Q -https://en.wikipedia.org/wiki/Quickhull
        convexHull = [farthestPoint]

        -- Three points P, Q, and C partition the remaining points of Sk into 3 subsets: S0, S1, and S2 
        --   where S0 are points inside triangle PCQ, S1 are points on the right side of the oriented
        --   line from P to C, and S2 are points on the right side of the oriented line from C to Q
        -- -https://en.wikipedia.org/wiki/Quickhull
        subset0 = filter (pointInsideTriangle (px, py) farthestPoint (qx, qy)) inputList
        subset1 = filter (\point -> (sideOfLine ((px, py),farthestPoint) point) == Algorithms.Right) inputList
        subset2 = filter (\point -> (sideOfLine (farthestPoint,(qx, qy)) point) == Algorithms.Right) inputList

-- Executes the QuickHull Convex Hull Algorithm
-- Input: the list of points you want the convex hull of
-- Output: the result of the calculation
-- Algorithm from https://en.wikipedia.org/wiki/Quickhull
quickHull :: [Point2D] -> [Point2D]
quickHull input =
    -- Find the left and rightmost points
    -- Divide the input points (aside from the left and rightmost points) into two sides
    filteredInput
    --[leftMostPoint, rightMostPoint] ++ hullLeft ++ hullRight
    where
        leftMostPoint = Data.List.minimumBy(farthest Algorithms.Right) input
        rightMostPoint = Data.List.maximumBy(farthest Algorithms.Right) input 
        line = (leftMostPoint, rightMostPoint)
        filteredInput = removeFromList rightMostPoint (removeFromList leftMostPoint input)
        leftSegment = filter (\point -> (sideOfLine line point) == Algorithms.Left) filteredInput
        rightSegment = filter (\point -> (sideOfLine line point) == Algorithms.Right) filteredInput
        hullLeft = findHull leftSegment (leftMostPoint, rightMostPoint)
        hullRight = findHull rightSegment (rightMostPoint, leftMostPoint)