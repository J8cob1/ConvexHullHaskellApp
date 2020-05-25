module Interactive where

import Algorithms
import Data.Char(digitToInt) -- https://stackoverflow.com/questions/53186296/converting-char-to-int-in-haskell

-- Data for the user facing part of this application
datasets = [
    [(4.0,5.0),(5.0,4.0),(9.0,2.0),(9.0,10.0),(8.0,5.0),(3.0,6.0),(2.0,4.0),(8.0,9.0),(1.0,1.0),(0.0,0.0),(7.0,6.0),(0.0,2.0)],
    [(4.0,5.0),(3.0,6.0),(1.0,9.0),(2.0,8.0),(3.0,7.0),(4.0,6.0),(5.0,5.0),(4.0,3.0),(2.0,8.0),(0.0,10.0),(15.0,7.0),(1.0,8.0)],
    [(93.0,32.0),(65.0,65.0),(24.0,32.0),(9.0,33.0),(98.0,21.0),(23.0,43.0),(32.0,23.0),(23.0,23.0),(23.0,23.0),(22.0,23.0),(24.0,23.0),(23.0,23.0)]]

-- A function to help us execute our convex hull algorithms in the (terminal) UI of the application
-- -Input: 
--   * algorithmSelection: a stringified integer indicating what algorithm you want to run 
--   * inputSelection: a stringified integer used to specify what data set you want to execute your selected algorithm on
-- -Output: the restulf of the algroithm's execution 
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
                "1" -> datasets!!0 -- https://stackoverflow.com/questions/5217171/how-can-i-get-nth-element-from-a-list
                "2" -> datasets!!1
                _ -> datasets!!2

-- A function that takes a stringified version of a list of points and parses it into a list of points
-- https://stackoverflow.com/questions/919937/convert-a-string-list-to-an-int-list and https://stackoverflow.com/questions/53186296/converting-char-to-int-in-haskell and https://wiki.haskell.org/Converting_numbers referenced
-- Input: the string representing the list of points you want unstringified
-- Output: the list of points
-- List parsing does everything!
pointListUnstringify :: [Char] -> [Point2D]
pointListUnstringify chars =
    case chars of
        -- One tuple in string
        (paren1:firstNumber:comma:secondNumber:paren2:[]) ->     
            [(firstNumberAsPoint, secondNumberAsPoint)]
            where
                firstNumberAsPoint = realToFrac (digitToInt firstNumber)
                secondNumberAsPoint = realToFrac (digitToInt secondNumber)
        -- Multiple tuples in string. This function is recursive
        (paren1:firstNumber:comma:secondNumber:paren2:xs) -> 
            [(firstNumberAsPoint, secondNumberAsPoint)] ++ pointListUnstringify xs
            where
                firstNumberAsPoint = realToFrac (digitToInt firstNumber)
                secondNumberAsPoint = realToFrac (digitToInt secondNumber)
        -- Anything else
        _ -> []
