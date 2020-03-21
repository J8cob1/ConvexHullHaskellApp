module Interactive where

import Algorithms

-- Data for the user facing part of this application
exampleInput1 = [(4.0,5.0),(5.0,4.0),(9.0,2.0),(9.0,10.0),(8.0,5.0),(3.0,6.0),(2.0,4.0),(8.0,9.0),(1.0,1.0),(0.0,0.0),(7.0,6.0),(0.0,2.0)]
exampleInput2 = [(4.0,5.0),(3.0,6.0),(1.0,9.0),(2.0,8.0),(3.0,7.0),(4.0,6.0),(5.0,5.0),(4.0,3.0),(2.0,8.0),(0.0,10.0),(15.0,7.0),(1.0,8.0)]
exampleInput3 = [(93.0,32.0),(645.0,65.0),(24.0,32.0),(9.0,33.0),(908.0,231.0),(234.0,43.0),(32.0,23.0),(23.0,23.0),(23.0,23.0),(22.0,234.0),(243.0,23.0),(23.0,234.0)]

-- A function to help us execute our convex hull algorithms in the (terminal) UI of the application
-- Input: 
--   * algorithmSelection: a stringified integer indicating what algorithm you want to run 
--   * inputSelection: a stringified integer used to specify what data set you want to execute your selected algorithm on
-- Output: the restulf of the algroithm's execution 
runAlgorithm :: String -> String -> [Point2D]
runAlgorithm algorithmSelection inputSelection =
    algorithm points
    where
        algorithm =
            case algorithmSelection of
                "1" -> jarvisMarch
                _ -> grahamsScan
        points =
            case inputSelection of
                "1" -> exampleInput1
                "2" -> exampleInput2
                _ -> exampleInput3