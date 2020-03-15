{-
- File: UnitTest.hs
- Description: Implements unit tests for the algorithms
- Date: 3/15/2020
-}

{- 
- References:
-}

import Algorithms
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- To test for algorithm correctness
type testDataPoints = [(0,0),(4,4),(2,4),(2,2),(1,1))]
type testDataHull = [(0,0),(2,2),(2,4),(4,4)]

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

-- Pretty much copied and modified from the hw1 test spec :)
-- Not sure how all this works yet
spec :: Spec
spec = do 
    prop "Graham's Scan Functions Correctly" $ \_ -> (grahamScan testDataPoints) == testDataHull
    prop "Jarvis March Functions Correctly" $ \_ -> (jarvisMarch testDataPoints) == testDataHull