-- Some sources I referenced:
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
-- https://stackoverflow.com/questions/59618016/haskell-cant-import-system-random

module Interactive where

import Algorithms
import Charting
import Data.Char(digitToInt) -- https://stackoverflow.com/questions/53186296/converting-char-to-int-in-haskell
import Data.Tuple
import Data.List
import Criterion
import System.Random

-- Data for the user facing part of this application. This is what we start with, we can add more
datasets = [
    [(4.0,5.0),(5.0,4.0),(9.0,2.0),(9.0,10.0),(8.0,5.0),(3.0,6.0),(2.0,4.0),(8.0,9.0),(1.0,1.0),(0.0,0.0),(7.0,6.0),(0.0,2.0)],
    [(4.0,5.0),(3.0,6.0),(1.0,9.0),(2.0,8.0),(3.0,7.0),(4.0,6.0),(5.0,5.0),(4.0,3.0),(2.0,8.0),(0.0,10.0),(15.0,7.0),(1.0,8.0)],
    [(93.0,32.0),(65.0,65.0),(24.0,32.0),(9.0,33.0),(98.0,21.0),(23.0,43.0),(32.0,23.0),(23.0,23.0),(23.0,23.0),(22.0,23.0),(24.0,23.0),(23.0,23.0)]]

-- Algorithms
algorithms = [
    (1, "Jarvis March", jarvisMarch),
    (2, "Graham's Scan", grahamsScan),
    (3, "Quick Hull", quickHull)] -- https://en.wikibooks.org/wiki/Haskell/Higher-order_functions - eh

-- Functions for getting the first, second and third elements out of our algorithms tuples
-- Input: none
-- Output: the first, second or third parts of the tuple
---------------------------------------------------------------------------------------------------------
getfirst :: (Int, String, [Point2D] -> [Point2D]) -> String
getfirst (a,b,c) = show a -- https://stackoverflow.com/questions/2784271/haskell-converting-int-to-string
--
getSecond :: (Int, String, [Point2D] -> [Point2D]) -> String
getSecond (a,b,c) = b
--
getThird :: (Int, String, [Point2D] -> [Point2D]) -> [Point2D] -> [Point2D]
getThird (a,b,c) = c

-- A function to help us execute a specific convex hull algorithm on a specific dataset
-- Input: 
--   * algorithmSelection: a stringified integer indicating what algorithm you want to run 
--   * inputSelection: a stringified integer used to specify what data set you want to execute your selected algorithm on
-- Output: 
runAlgorithm :: [[Point2D]] -> String -> String -> IO ()
runAlgorithm given_datasets algorithmSelection inputSelection =
    runAlgorithmAndDrawChart (algorithms!!(algorithmNum-1)) (datasetNum, (given_datasets!!(datasetNum-1))) -- https://stackoverflow.com/questions/5217171/how-can-i-get-nth-element-from-a-list
    where
        algorithmNum = read algorithmSelection :: Int
        datasetNum = read inputSelection :: Int

-- A function that verifies user input for selection of an algorithm and dataset to run
-- Input:
--   * given_datasets - a set of datasets that the user is choosing from
--   * algorithm_selection - a string representing what algorithm the user chose
--   * datasetSelection - a string that indicates what item of the dataset the user chose
-- Output: whether the user made an appropriate algorithm and datset selection or not
verifySelection :: [[Point2D]] -> String -> String -> Bool
verifySelection given_datasets algorithmSelection datasetSelection =
    if (valid) then
        True
    else
        False
    where
        algorithmNum = read algorithmSelection :: Int
        datasetNum = read datasetSelection :: Int
        valid = 
            algorithmNum <= (length algorithms) && algorithmNum > 0 &&
            datasetNum <= (length given_datasets) && datasetNum > 0 -- https://stackoverflow.com/questions/5710078/in-haskell-performing-and-and-or-for-boolean-functions

-- Executes an algorithm on a dataset and returns the results, formatted as a string. This is a part of the interactive program that executes all of the algorithms on all of the datasets
-- Input:
--   * algorithm - the algorithm you want to run, as a tuple element of our algorithms list
--   * input_datasets - a list of datasets you want to run the algorithm on
-- Output: an IO object that contains the results of the operation
runAlgorithmOnDatasets :: (Int, String, [Point2D] -> [Point2D]) -> [[Point2D]] -> String 
runAlgorithmOnDatasets algorithm input_datasets =
    concat (map (\dataset -> "Result of " ++ (getSecond algorithm) ++ " on dataset " ++ show dataset ++ "\n  " ++ show ((getThird algorithm) dataset) ++ "\n\n") input_datasets) -- https://stackoverflow.com/questions/36937302/concatenate-a-list-of-strings-to-one-string-haskell

-- Runs a benchmarking operation on an algorithm executing on specific dataset
-- Input:
--   * algorithm - the algorithm you want to run, as a tuple element of our algorithms list
--   * dataset - the dataset you want to run the algorithm on
-- Output: an IO object that contains the results of the operation
benchmarkAlgorithmOnDataset :: (Int, String, [Point2D] -> [Point2D]) -> [Point2D] -> IO ()
benchmarkAlgorithmOnDataset algorithm dataset = do
    putStr ("Benchmark of " ++ (getSecond algorithm) ++ " on dataset: " ++ show dataset ++ ":\n")
    Criterion.benchmark (Criterion.whnf (getThird algorithm) dataset)
    putStrLn ""

-- Runs and algorithm, prints the results, and draws a chart showing the results of each run
-- Input:
--   * algorithm - the algorithm you want to run, as a tuple element of our algorithms list
--   * dataset - the dataset you want to run the algorithm on. Comes as a tuple. The first part is an index to represent the list, the second is a tuple
-- Output: an IO object we need to return so it can get executed in the main function
runAlgorithmAndDrawChart :: (Int, String, [Point2D] -> [Point2D]) -> (Int, [Point2D]) -> IO ()
runAlgorithmAndDrawChart algorithm dataset = do
    putStr ("Result of " ++ (getSecond algorithm) ++ " on dataset " ++ show input ++ "\n  " ++ show result ++ "\n\n")
    drawChart 
        (input \\ result) -- All points not in the hull
        result  -- Points in the hull, with the head added to the end so that we draw a complete polygon
        ("Result of " ++ (getSecond algorithm) ++ " on Dataset " ++ show index) -- Chart Title
        ((getSecond algorithm) ++ " on dataset " ++ show index) -- Chart Filename
    where
        result = ((getThird algorithm) input)
        input = snd dataset -- https://stackoverflow.com/questions/5844347/accessing-a-specific-element-in-a-tuple
        index = fst dataset

-- Run all of the algorithms we have defined on the datsets we are given
-- Input: input_datasets - a list of points you want to run all the algorithms on
-- Output: the output of the algorithms processed 
runAllAlgorithms :: [[Point2D]] -> IO ()
runAllAlgorithms input_datasets = do
    putStrLn "Results\n-------"
    chartdraw -- draw charts
    putStrLn "\nBenchmarks\n----------"
    benchmarks -- show benchmarks
    where
        chartdraw = mapM_ (\algorithm -> mapM_ (runAlgorithmAndDrawChart algorithm) (zip [1..] input_datasets)) algorithms -- https://stackoverflow.com/questions/9749904/what-is-a-good-way-to-generate-a-infinite-list-of-all-integers-in-haskell
        benchmarks = mapM_ (\algorithm -> mapM_ (benchmarkAlgorithmOnDataset algorithm) input_datasets) algorithms -- https://stackoverflow.com/questions/45194657/how-do-i-run-through-a-list-with-an-io-operation

-- Transoforms a list of list of points into a string, for clean printing
-- Input:
--   * num - a number to display next to the current 
--   * (x:xs) - the list of lists we are printing from, x being the one that we are printing in this iteration of the function
-- Output: the stringified version of the list of list of points
pointListToCleanStr :: Int -> [[Point2D]] -> String
pointListToCleanStr num [] = ""
pointListToCleanStr num (x:xs) =
    show num ++ ". " ++ (show x) ++ "\n" ++ (pointListToCleanStr (num+1) xs) -- needs 1. nums

-- Returning a string represenation of the list of our convex hull algorithms
-- Input:
--   * (x:xs) - the list of algorithms we want to print out, in the form of a custom 3 item tuple
-- Output: a stringified version of our convex hull algorithms
displayAlgorithms :: [(Int, String, [Point2D] -> [Point2D])] -> String
displayAlgorithms [] = ""
displayAlgorithms (x:xs) =
    (getfirst x) ++ " - " ++ (getSecond x) ++ ['\n'] ++ displayAlgorithms xs -- https://wiki.haskell.org/How_to_work_on_lists

-- A function that takes a stringified version of a list of points and parses it into a list of points
-- https://stackoverflow.com/questions/919937/convert-a-string-list-to-an-int-list and https://stackoverflow.com/questions/53186296/converting-char-to-int-in-haskell and https://wiki.haskell.org/Converting_numbers referenced
-- Input: the string representing the list of points you want unstringified
-- Output: the list of points
-- List parsing does everything!
-- Terrible
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

-- Displays the main options of our list. You could just as easily put this in the main function, but it's here at the moment
-- Input: none
-- Output: the main options in our program, as a string
displayMainOptions :: String
displayMainOptions = 
    -- https://stackoverflow.com/questions/22918837/how-can-i-write-multiline-strings-in-haskell
    "--------------------------------------------------- \
    \ \nWhat would you like to do? \
    \ \n    1. View available datasets \
    \ \n    2. Add a dataset \
    \ \n    3. Randomly generate a new dataset \
    \ \n    4. Select an algorithm and a data set you want to run the algorithm on \
    \ \n    5. Run all of the available algorithms on all the available datasets \
    \ \n    6. Exit \
    \ \n\nEnter Selection as an integer\
    \ \n"

-- Generates a random list of points
-- Input: 
--   * upperLimit - the upper limit of the x and y coordinates that can be generated for this list
--   * lowerLimit - the lower limit of the x and y coordinates that can be generated for this list
--   * numPoints - the number of points you want in the list
-- Output: the dataset that was just generated
-- https://hackage.haskell.org/package/random-1.1/docs/System-Random.html#v:next
-- https://stackoverflow.com/questions/42975088/how-to-convert-from-float-to-int-in-haskell
-- https://stackoverflow.com/questions/36903738/no-instance-for-integral-double-arising-from-a-use-of-rem
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#t:Integral
generateRandomDataSet :: String -> String -> Int -> IO [Point2D]
generateRandomDataSet upperLimit lowerLimit numPoints = do
    if (numPoints > 0) then do
        xCoord <- getStdRandom (randomR (lower,upper))
        yCoord <- getStdRandom (randomR (lower,upper))
        otherPoints <- (generateRandomDataSet upperLimit lowerLimit (numPoints - 1))
        return ([(fromIntegral (round xCoord), fromIntegral (round yCoord))] ++ otherPoints) -- https://stackoverflow.com/questions/18280844/converting-integer-to-double-in-haskell#:~:text=1%20Answer&text=The%20usual%20way%20to%20convert,which%20Double%20is%20an%20instance.
    else
        return []
    where
        lower = read lowerLimit :: Number -- https://stackoverflow.com/questions/20667478/haskell-string-int-type-conversion
        upper = read upperLimit :: Number