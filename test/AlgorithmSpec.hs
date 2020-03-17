module AlgorithmSpec where
{-
- File: UnitTest.hs
- Description: Implements unit tests for the algorithms
- Date: 3/15/2020
-}

{- 
- References:
- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html
- https://stackoverflow.com/questions/940382/what-is-the-difference-between-dot-and-dollar-sign
-}

import Algorithms
import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- Data: to use for testing
testDataPoints1 :: [Point2D] 
testDataPoints1 = []

testDataPoints2 :: [Point2D] 
testDataPoints2 = [(0,0),(4,4)]

testDataPoints3 :: [Point2D] 
testDataPoints3 = [(0,0),(4,4),(2,4)]

testDataPoints4 :: [Point2D] 
testDataPoints4 = [(-1,-1),(5,5),(-1,5),(5,-1),(1,1),(2,3),(4,1),(0,1)]

testDataHull1 :: [Point2D]
testDataHull1 = []

testDataHull2 :: [Point2D]
testDataHull2 = []

testDataHull3 :: [Point2D]
testDataHull3 = [(0,0),(2,4),(4,4)]

testDataHull4 :: [Point2D]
testDataHull4 = [(-1,-1),(5,5),(-1,5),(5,-1)]

-- A function that tests the security of a convex hull alg
testConvexHullAlgorithm :: ([Point2D] -> [Point2D]) -> [Point2D] -> [Point2D] -> Bool
testConvexHullAlgorithm algorithm points expectedHull = 
    (Data.List.sort expectedHull) == resultSet
    where
        resultSet = Data.List.sort (algorithm points)

testRemoveList :: Bool
testRemoveList = 
    (Data.List.elem (4,4) listWithElementRemoved) == False
    where
        listWithElementRemoved = removeFromList (4,4) testDataPoints3

testRemoveDupsFromList :: Bool
testRemoveDupsFromList = 
    listWithDupsRemoved == [(-1,-1),(4,5)]
    where
        listWithDupsRemoved = removeDupsFromList (-1,-1) [(-1,-1),(2,2),(4,4),(1,1),(3,3),(4,5)]

-- Test data
-- https://www.mathsisfun.com/polar-cartesian-coordinates.html used as reference
start = (0, 0)
p1 = (4, 4)
p2 = (4, 10)
p3 = (5, 5)

testPolarAnglePosGT :: Bool
testPolarAnglePosGT = 
    -- p2 has a greater polar angle than p1 with respect to start and the positive x-axis
    (polarAngle False start p2 p1 == GT)

testPolarAnglePosLT :: Bool
testPolarAnglePosLT =
    -- p1 has a lesser polar angle with respect to start and a positive x-axis than p2
    (polarAngle False start p1 p2 == LT)

testPolarAnglePosEQ :: Bool
testPolarAnglePosEQ = 
    -- p1 and p2 have equal polar angles with respect to the start point and the positive x axis
    (polarAngle False start p1 p3 == EQ)

-- Pretty much copied and modified from the hw1 test spec :)
-- Not sure how all this works yet
spec :: Spec
spec = do 
    prop "testRemoveList" $ testRemoveList
    prop "removeDupsFromList" $ testRemoveList
    
    prop "polarAngle greater than on positive x-axis" $ testPolarAnglePosGT --
    prop "polarAngle less than on positive x-axis" $ testPolarAnglePosLT --
    prop "polarAngle equal to on positive x-axis" $ testPolarAnglePosEQ --

    -- Don't let these decieve you. Graham's scan is broken
    prop "Graham's Scan on an empty list" $ (testConvexHullAlgorithm grahamScan testDataPoints1 testDataHull1)
    prop "Graham's Scan on a set of insufficent points" $ (testConvexHullAlgorithm grahamScan testDataPoints2 testDataHull2)
    prop "Graham's Scan on a three item set that is already a convex hull" $ (testConvexHullAlgorithm grahamScan testDataPoints3 testDataHull3)
    prop "Graham's Scan on a normal set of points" $ (testConvexHullAlgorithm grahamScan testDataPoints4 testDataHull4)

    -- Don't let these decieve you. Jarvis March is broken
    prop "Jarvis March Empty List" $ (testConvexHullAlgorithm jarvisMarch testDataPoints1 testDataHull1)
    prop "Jarvis March Insufficient Points" $ (testConvexHullAlgorithm jarvisMarch testDataPoints1 testDataHull1)
    prop "Jarvis March Already-A-Hull" $ (testConvexHullAlgorithm jarvisMarch testDataPoints1 testDataHull1)
    prop "Jarvis March Normal-Exec" $ (testConvexHullAlgorithm jarvisMarch testDataPoints1 testDataHull1)

