-- Referenced:
-- https://stackoverflow.com/questions/53186296/converting-char-to-int-in-haskell
-- https://wiki.haskell.org/How_to_work_on_lists
-- https://wiki.haskell.org/Cookbook/Lists_and_strings
-- https://stackoverflow.com/questions/5952167/how-do-i-print-a-list-in-haskell - a little maybe?
-- https://stackoverflow.com/questions/22220439/haskell-lambda-expression
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-IORef.html#g:1
-- https://en.wikibooks.org/wiki/Haskell/Higher-order_functions - eh
-- https://stackoverflow.com/questions/2784271/haskell-converting-int-to-string
-- https://stackoverflow.com/questions/5217171/how-can-i-get-nth-element-from-a-list
-- https://stackoverflow.com/questions/5710078/in-haskell-performing-and-and-or-for-boolean-functions
-- https://stackoverflow.com/questions/919937/convert-a-string-list-to-an-int-list
-- https://stackoverflow.com/questions/53186296/converting-char-to-int-in-haskell
-- https://wiki.haskell.org/Converting_numbers referenced
-- https://stackoverflow.com/questions/22918837/how-can-i-write-multiline-strings-in-haskell
-- https://stackoverflow.com/questions/45194657/how-do-i-run-through-a-list-with-an-io-operation 
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-List.html#v:concat

module Interactive where

import Algorithms
import Data.Char(digitToInt) -- https://stackoverflow.com/questions/53186296/converting-char-to-int-in-haskell
import Data.Tuple
import Data.List(concat)
import Criterion

-- Data for the user facing part of this application. This is what we start with, we can add more
datasets = [
    [(4.0,5.0),(5.0,4.0),(9.0,2.0),(9.0,10.0),(8.0,5.0),(3.0,6.0),(2.0,4.0),(8.0,9.0),(1.0,1.0),(0.0,0.0),(7.0,6.0),(0.0,2.0)],
    [(4.0,5.0),(3.0,6.0),(1.0,9.0),(2.0,8.0),(3.0,7.0),(4.0,6.0),(5.0,5.0),(4.0,3.0),(2.0,8.0),(0.0,10.0),(15.0,7.0),(1.0,8.0)],
    [(93.0,32.0),(65.0,65.0),(24.0,32.0),(9.0,33.0),(98.0,21.0),(23.0,43.0),(32.0,23.0),(23.0,23.0),(23.0,23.0),(22.0,23.0),(24.0,23.0),(23.0,23.0)]]

-- Algorithms
algorithms = [
    (1, "Jarvis March", jarvisMarch),
    (2, "Graham's Scan", grahamsScan),
    (3, "We don't know yet!", id)] -- https://en.wikibooks.org/wiki/Haskell/Higher-order_functions - eh

-- Functions for getting the first, second and third elements out of our tuples
-- Input: 
--   *
--   *
-- Output:
---------------------------------------------------------------------------------------------------------
getfirst :: (Int, String, [Point2D] -> [Point2D]) -> String
getfirst (a,b,c) = show a -- https://stackoverflow.com/questions/2784271/haskell-converting-int-to-string

getSecond :: (Int, String, [Point2D] -> [Point2D]) -> String
getSecond (a,b,c) = b

getThird :: (Int, String, [Point2D] -> [Point2D]) -> [Point2D] -> [Point2D]
getThird (a,b,c) = c

-- A function to help us execute our convex hull algorithms in the (terminal) UI of the application
-- Input: 
--   * algorithmSelection: a stringified integer indicating what algorithm you want to run 
--   * inputSelection: a stringified integer used to specify what data set you want to execute your selected algorithm on
-- Output: the restulf of the algroithm's execution 
runAlgorithm :: String -> String -> [Point2D]
runAlgorithm algorithmSelection inputSelection =
    if valid then 
        (getThird (algorithms!!algorithmNum)) (datasets!!pointNum) -- https://stackoverflow.com/questions/5217171/how-can-i-get-nth-element-from-a-list
    else
        []
    where
        algorithmNum = digitToInt (algorithmSelection!!0)
        pointNum = digitToInt (inputSelection!!0)
        valid = 
            algorithmNum <= (length algorithms) && algorithmNum > 0 &&
            pointNum <= (length datasets) && pointNum > 0 -- https://stackoverflow.com/questions/5710078/in-haskell-performing-and-and-or-for-boolean-functions
 
--getAlgorithms :: [[Point2D] -> [Point2D]]
--getAlgorithms = map getThird algorithms

-- Executes an algorithm on a dataset and returns the results, formatted as a string
-- Input:
--   *
--   *
-- Output: an IO object that contains the results of the operation
runAlgorithmOnDatasets :: (Int, String, [Point2D] -> [Point2D]) -> [[Point2D]] -> String 
runAlgorithmOnDatasets algorithm input_datasets =
    concat (map (\dataset -> "Result of " ++ (getSecond algorithm) ++ " on dataset " ++ show dataset ++ "\n  " ++ show ((getThird algorithm) dataset) ++ "\n\n") input_datasets) -- https://stackoverflow.com/questions/36937302/concatenate-a-list-of-strings-to-one-string-haskell

-- Runs a benchmarking operation on a list given
-- Input:
--   *
--   *
-- Output: an IO object that contains the results of the operation
benchmarkAlgorithmOnDataset :: (Int, String, [Point2D] -> [Point2D]) -> [Point2D] -> IO ()
benchmarkAlgorithmOnDataset algorithm dataset = do
    putStr ("Benchmark of " ++ (getSecond algorithm) ++ " on dataset: " ++ show dataset ++ ":\n")
    Criterion.benchmark (Criterion.whnf (getThird algorithm) dataset)
    putStrLn ""

-- Run all of the algorithms we have defined on the datsets we are given
-- Input: input_datasets - a list of points you want to run all the algorithms on
-- Output: - the output of the algorithms processed 
runAllAlgorithms :: [[Point2D]] -> IO ()
runAllAlgorithms input_datasets = do
    putStrLn "Results\n-------"
    putStr (concat results)
    putStrLn "\nBenchmarks\n----------"
    benchmarks
    where
        results = map (\algorithm -> runAlgorithmOnDatasets algorithm input_datasets) algorithms
        benchmarks = mapM_ (\algorithm -> mapM_ (benchmarkAlgorithmOnDataset algorithm) input_datasets) algorithms -- https://stackoverflow.com/questions/45194657/how-do-i-run-through-a-list-with-an-io-operation

-- Runs a benchmarking operation on a list given
-- Input:
--   *
--   *
-- Output: an IO object that contains the results of the operation
pointListToCleanStr :: [[Point2D]] -> String
pointListToCleanStr [] = ""
pointListToCleanStr (x:xs) =
    (show x) ++ ['\n'] ++ pointListToCleanStr xs -- needs 1. nums

-- Runs a benchmarking operation on a list given
-- Input:
--   *
--   *
-- Output: an IO object that contains the results of the operation
algorithmAndResultToString :: (Int, String, [Point2D] -> [Point2D]) -> [Point2D] -> String
algorithmAndResultToString algoEntry result =
    (getSecond algoEntry) ++ ['\n'] ++ show result -- copied displayAlgorithms and modified it

-- Runs a benchmarking operation on a list given
-- Input:
--   *
--   *
-- Output: an IO object that contains the results of the operation
displayAlgorithms :: [(Int, String, [Point2D] -> [Point2D])] -> String
displayAlgorithms [] = ""
displayAlgorithms (x:xs) =
    (getfirst x) ++ " - " ++ (getSecond x) ++ ['\n'] ++ displayAlgorithms xs -- https://wiki.haskell.org/How_to_work_on_lists

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

-- Displays the main options of our list
-- Input:
--   *
--   *
-- Output: an IO object that contains the results of the operation
displayMainOptions :: String
displayMainOptions = 
    -- https://stackoverflow.com/questions/22918837/how-can-i-write-multiline-strings-in-haskell
    "Welcome to the Convex Hull App \
    \ \n--------------------------------------------------- \
    \ \n What would you like to do? \
    \ \n     1. View available datasets \
    \ \n     2. Add a dataset \
    \ \n     3. Select an algorithm and a data set you want to run the algorithm on \
    \ \n     4. Run all of the available algorithms on all the available datasets \
    \ \n     5. Exit \
    \ \n Enter Selection (as an integer): "