{-
- File: Algorithms.hs
- Date: 3/20/2020
- Description: contains the Convex Hull Algorithms used in this project
-}

module Algorithms where

-- Imports
import Data.List

-- Typedefs
type Number = Double
type Point2D = (Number, Number)

{- Functions Common to one or more algorithms -}
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
angle :: Bool -> Point2D -> Point2D -> (Number, Point2D)
angle axis (x1, y1) (x2, y2) =
    if (x1 == x2 && y1 == y2) then 
        (20, (x2, y2))
    else 
        case axis of
            True -> (atan2 (y2 - y1) (x2 - x1), (x2, y2))
            False -> (atan2 (y1 - y2) (x2 - x1), (x2, y2))

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

jarvisMarchGetChain :: Point2D -> Point2D -> Bool -> [Point2D] -> [Point2D]
jarvisMarchGetChain start end isRightChain coords = 
    if (hullPoint == end || hullPoint == start) then
        []
    else
        [hullPoint] ++ (jarvisMarchGetChain hullPoint end isRightChain coords)
    where 
        hullPoint = ((\(x, y) -> y) (Data.List.minimum (map (angle isRightChain start) coords)))


jarvisMarchHull :: [Point2D] -> [Point2D]
jarvisMarchHull coords =
    [minPoint] ++ (jarvisMarchGetChain minPoint maxPoint True coords) 
    -- ++ [maxPoint] 
    -- ++ (jarvisMarchGetChain maxPoint minPoint maxPoint False coords)
    where
        maxPoint = Data.List.maximum(coords)
        minPoint = Data.List.minimum(coords)

--jarvisMarchHull [(0.0, 0.0), (1.0, 1.0), (2.0, 0.0), (2.0, 3.0), (0.0, 3.0), (1.0, 2.0)]
-- Calculate polar angle form starting point for every point in coords (with a map), then get the max of that 
-- [(0.0, 0.0), (1.1, 1.1), (2.2,2.2)]
