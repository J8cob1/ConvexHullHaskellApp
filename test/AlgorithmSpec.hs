module AlgorithmSpec where
{-
- File: UnitTest.hs
- Description: Implements unit tests for the algorithms. Not sure how effective these are, but at least I got to play with them a little bit
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


----------------------------------------------
----------------- Testing Data  --------------
----------------------------------------------
testDataPoints1 :: [Point2D] 
testDataPoints1 = []

testDataPoints2 :: [Point2D] 
testDataPoints2 = [(0,0),(4,4)]

testDataPoints3 :: [Point2D] 
testDataPoints3 = [(0,0),(4,4),(2,4)]

testDataPoints4 :: [Point2D] 
testDataPoints4 = [(-1,-1),(5,5),(-1,5),(5,-1),(1,1),(2,3),(4,1),(0,1)]

testDataPoints5 :: [Point2D]
testDataPoints5 = [(4.0,5.0),(3.0,6.0),(1.0,9.0),(2.0,8.0),(3.0,7.0),(4.0,6.0),(5.0,5.0),(4.0,3.0),(2.0,8.0),(0.0,10.0),(15.0,7.0),(1.0,8.0)]

testDataPoints6 :: [Point2D]
testDataPoints6 = [(4.0,5.0),(5.0,4.0),(9.0,2.0),(9.0,10.0),(8.0,5.0),(3.0,6.0),(2.0,4.0),(8.0,9.0),(1.0,1.0),(0.0,0.0),(7.0,6.0),(0.0,2.0)]

testDataHull1 :: [Point2D]
testDataHull1 = []

testDataHull2 :: [Point2D]
testDataHull2 = []

testDataHull3 :: [Point2D]
testDataHull3 = [(0,0),(2,4),(4,4)]

testDataHull4 :: [Point2D]
testDataHull4 = [(-1,-1),(5,5),(-1,5),(5,-1)]

testDataHull5 :: [Point2D]
testDataHull5 = [(4, 3), (1, 8), (0, 10), (15, 7)]

testDataHull6 :: [Point2D]
testDataHull6 = [(0,0),(0,2),(3,6),(9,10),(9,2)]


----------------------------------------------
-------------- Helper Functions --------------
----------------------------------------------

-- A function to help us run Convex Hull algorithms in "spec" test at the bottom of the page
-- -Input: 
--   * algorithm: the algorithm you want to run, as a ([Point2D] -> [Point2D])
--   * points: the input you want to run the convex hull algorithm on, as a [Point2D]
--   * expectedHull: the convex hull you expected  [Point2D]
-- -Output: true if running the selected algorithm on the given set of points resulted in a set of points that matched the expectedHull
testConvexHullAlgorithm :: ([Point2D] -> [Point2D]) -> [Point2D] -> [Point2D] -> Bool
testConvexHullAlgorithm algorithm points expectedHull = 
    (Data.List.sort expectedHull) == resultSet
    where
        resultSet = Data.List.sort (algorithm points)

-- A function to help us test the removeList functions. I think I might have had problems with lambdas
-- -Input: none
-- -Output: true if running removeList successfully removed an item from a list. This test only tests one case
testRemoveList :: Bool
testRemoveList = 
    (Data.List.elem (4,4) listWithElementRemoved) == False
    where
        listWithElementRemoved = removeFromList (4,4) testDataPoints3

-- A function to help us test the removePolarAngleDupsFromList function
-- -Input: none
-- -Output: true if running removeList successfully removes all the duplicate items in a list stored in this function
testRemovePolarAngleDupsFromList :: Bool
testRemovePolarAngleDupsFromList = 
    listWithDupsRemoved == [(-1,-1),(4,5)]
    where
        listWithDupsRemoved = removePolarAngleDupsFromList (-1,-1) [(-1,-1),(2,2),(4,4),(1,1),(3,3),(4,5)]

-- Test data for polar angle function tests
-- https://www.mathsisfun.com/polar-cartesian-coordinates.html used as reference
start = (0, 0)
p1 = (4, 4)
p2 = (4, 10)
p3 = (5, 5)

-- A function to help us test the polarAngle function. It should return GT in this scenario
-- -Input: none
-- -Output: true if function returned the expected result, False otherwise
testPolarAnglePosGT :: Bool
testPolarAnglePosGT = 
    -- p2 has a greater polar angle than p1 with respect to start and the positive x-axis
    (polarAngle False start p2 p1 == GT)

-- A function to help us test the polarAngle function. It should return LT in this scenario
-- -Input: none
-- -Output: true if function returned the expected result, False otherwise
testPolarAnglePosLT :: Bool
testPolarAnglePosLT =
    -- p1 has a lesser polar angle with respect to start and a positive x-axis than p2
    (polarAngle False start p1 p2 == LT)

-- A function to help us test the polarAngle function. It should return EQ in this scenario
-- -Input: none
-- -Output: true if function returned the expected result, False otherwise
testPolarAnglePosEQ :: Bool
testPolarAnglePosEQ = 
    -- p1 and p2 have equal polar angles with respect to the start point and the positive x axis
    (polarAngle False start p1 p3 == EQ)

-- Pretty much copied and modified from the hw1 test spec :)
spec :: Spec
spec = do 
    -- List function tests
    prop "testRemoveList" $ testRemoveList
    prop "removePolarAngleDupsFromList" $ testRemoveList
    
    -- Polar angle function tests
    prop "polarAngle greater than on positive x-axis" $ testPolarAnglePosGT
    prop "polarAngle less than on positive x-axis" $ testPolarAnglePosLT
    prop "polarAngle equal to on positive x-axis" $ testPolarAnglePosEQ

    -- Graham's scan tests
    prop "Graham's Scan on an empty list" $ (testConvexHullAlgorithm grahamsScan testDataPoints1 testDataHull1)
    prop "Graham's Scan on a set of insufficent points" $ (testConvexHullAlgorithm grahamsScan testDataPoints2 testDataHull2)
    prop "Graham's Scan on a three item set that is already a convex hull" $ (testConvexHullAlgorithm grahamsScan testDataPoints3 testDataHull3)
    prop "Graham's Scan on a normal set of points" $ (testConvexHullAlgorithm grahamsScan testDataPoints4 testDataHull4)
    prop "Graham's Scan on a different normal set of points" $ (testConvexHullAlgorithm grahamsScan testDataPoints5 testDataHull5)
    prop "Graham's Scan on a different normal set of points" $ (testConvexHullAlgorithm grahamsScan testDataPoints6 testDataHull6)

    -- Jarvis March tests
    prop "Jarvis March Empty List" $ (testConvexHullAlgorithm jarvisMarch testDataPoints1 testDataHull1)
    prop "Jarvis March Insufficient Points" $ (testConvexHullAlgorithm jarvisMarch testDataPoints2 testDataHull2)
    prop "Jarvis March Already-A-Hull" $ (testConvexHullAlgorithm jarvisMarch testDataPoints3 testDataHull3)
    prop "Jarvis March on a normal set of points" $ (testConvexHullAlgorithm jarvisMarch testDataPoints4 testDataHull4)
    prop "Jarvis March on a different normal set of points" $ (testConvexHullAlgorithm jarvisMarch testDataPoints5 testDataHull5)
    prop "Graham's Scan on a different normal set of points" $ (testConvexHullAlgorithm grahamsScan testDataPoints6 testDataHull6)