{-
- File: Algorithms.hs
- Date: 3/20/2020
- Description: contains the Convex Hull Algorithms used in this project
-}

module Algorithms where

-- Imports
import Data.List
import Prelude.Math
import cmath

-- Typedefs
type Integer = Int
type Point2D = (Integer, Integer)

{- Functions Common to one or more algorithms -}
-- Finds the point in the list with the lowest x and y coordinate
-- TODO: create QuickCheck test. Otherwise, it seems like it works
lowestPoint :: [Point2D] -> Point2D
lowestPoint listOfPoints = Data.List.minimum (listOfPoints)

highestPoint :: [Point2D] -> Point2D
highestPoint listOfPoints = Data.List.maximum (listOfPoints)

-- Calculates the angle between two points, with respect to either the negative or positive x axis
-- INPUT:
--     axis: a boolean indicating whether to use the positive x axis
--     first: a point indicating if the starting point
--     second: a
-- https://stackoverflow.com/questions/2339487/calculate-angle-of-2-points/2339510
-- https://docs.python.org/2/library/cmath.html
-- https://hackage.haskell.org/package/cmath
angle :: Integer -> Point2D -> Point2D
angle axis first second = 
    case axis of
        True -> cmath.atan (second[1] - first[1]) (second[0] - first[0])
        False -> cmath.atan (first[1] - second[1]) (second[0] - first[0])

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
jarvisMarchGetNextVertex :: Point2D -> Point2D -> Boolean -> [Point2D] -> [Point2D]
jarvisMarchGetNextVertex start end previous isRightChain coords = 
    case previous of
        end -> []
        _ -> [Data.List.minimum (map (\x -> angle isRightChain start x) coords)]

jarvisMarchHull :: [Point2D] -> [Point2D]
jarvisMarchHull coords = 
    [minPoint] 
    ++ (jarvisMarchGetNextVertex minPoint maxPoint minPoint True coords) 
    ++ [maxPoint] 
    ++ (jarvisMarchGetNextVertex maxPoint start maxPoint False coords)
    where
        maxPoint = maximum(coords)
        minPoint = min(coords)
-- Calculate polar angle form starting point for every point in coords (with a map), then get the max of that