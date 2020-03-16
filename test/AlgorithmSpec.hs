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

-- To test for algorithm correctness
testDataPoints :: [Point2D] 
testDataPoints = Data.List.sort [(0,0),(4,4),(2,4),(2,2),(1,1)]

testDataHull :: [Point2D]
testDataHull = Data.List.sort [(0,0),(2,2),(2,4),(4,4)]

-- Tests for both algorithms
-- enoughPoints 
-- removeFromList
-- polarAngle


-- Graham's Scan Tests
-- makesLeftTurn
-- gramScanCleanUpList
-- gramScanAddNextPoint

-- grahamScan <-- *

-- Jarvis March Tests
-- jarvisMarchConstructChain
-- jarvisMarch <-- *

testConvexHullAlgorithm :: ([Point2D] -> [Point2D]) -> Bool
testConvexHullAlgorithm algorithm = 
    length (sortedPoints Data.List.\\ testDataHull) == 0
    where
        sortedPoints = Data.List.sort (algorithm testDataPoints)

testRemoveList :: Bool
testRemoveList = 
    (Data.List.elem (4,4) listWithElementRemoved) == False
    where
        listWithElementRemoved = removeFromList (4,4) testDataPoints

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

{-testPolarAngleNegGT :: Bool
testPolarAngleNegGT = 
    -- p2 has a lesser polar angle than p1 with respect to start and the negative x-axis
    (polarAngle True start p2 p1 == LT)

testPolarAngleNegLT :: Bool
testPolarAngleNegLT =
    -- p1 has a greater polar angle with respect to start and the negative x-axis than p2
    (polarAngle True start p1 p2 == GT)

testPolarAngleNegEQ :: Bool
testPolarAngleNegEQ = 
    -- p1 and p2 have equal polar angles with respect to the start point and the negative x-axis
    (polarAngle True start p1 p3 == EQ)-}


-- Pretty much copied and modified from the hw1 test spec :)
-- Not sure how all this works yet
spec :: Spec
spec = do 
    prop "Graham's Scan Functions Correctly" $ (testConvexHullAlgorithm grahamScan)
    prop "Jarvis March Functions Correctly" $ (testConvexHullAlgorithm jarvisMarch)
    prop "Remove from list functions Correctly" $ testRemoveList
    prop "polarAngle greater than on positive x-axis works correctly" $ testPolarAnglePosGT
    prop "polarAngle less than on positive x-axis works correctly" $ testPolarAnglePosLT
    prop "polarAngle equal to on positive x-axis works correctly" $ testPolarAnglePosEQ
    --prop "polarAngle greater than on negative axis works correctly" $ testPolarAngleNegGT
    --prop "polarAngle less than on negative x-axis works correctly" $ testPolarAngleNegLT
    --prop "polarAngle equal to on negative x-axis works correctly" $ testPolarAngleNegEQ

