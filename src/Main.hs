module Main where

import Criterion -- for performance measurement
import Algorithms -- Contains convex hull algorithms
import Interactive -- Contains functions and data specific to the interactive portion of the app
import Charting
import Data.IORef
import System.Exit
import Data.Char(digitToInt)
import Data.List

-- Dataset
exec :: (IORef [[Point2D]]) -> IO ()
exec convexHullDataSets = do

    -- Print menu and get selection
    putStr(displayMainOptions)
    menuSelection <- getLine
    putStrLn ""

    -- Process the selection
    if (menuSelection == "1") then do
        -- View available data sets
        putStrLn "Available Data Sets:"
        datasetList <- readIORef convexHullDataSets -- https://stackoverflow.com/questions/5289779/printing-elements-of-a-list-on-new-lines ?
        putStrLn (pointListToCleanStr 1 datasetList)
        putStr "\n"

    else if (menuSelection == "2") then do
        -- Add a dataset, then print them all out again -- this is broken
        putStrLn "Enter dataset (in the form: (1,2)(3,4)... making sure to do it right. Enter anything wrong and the app can crash):"
        userInput <- getLine
        datasetList <- readIORef convexHullDataSets
        writeIORef convexHullDataSets (datasetList ++ [(pointListUnstringify userInput)])
        
        putStrLn "Available Data Sets:"
        datasetList <- readIORef convexHullDataSets -- https://stackoverflow.com/questions/5289779/printing-elements-of-a-list-on-new-lines ?
        putStrLn (pointListToCleanStr 1 datasetList)
        putStr "\n"
    
    else if (menuSelection == "3") then do
        -- Randomly generate a new dataset (easier than typing on out yourself)

        -- Get user Input
        putStrLn "Please enter X and Y coordinate upper limit, as an integer: "
        upperLimit <- getLine

        putStrLn "Please enter X and Y coordinate lower limit, as an integer: "
        lowerLimit <- getLine

        putStrLn "Please enter the number of points you want to generate, as an integer: "
        numPoints <- getLine

        -- Generate new set and put it into the list
        newDataSet <- generateRandomDataSet upperLimit lowerLimit (read numPoints :: Int)
        currentDataSet <- readIORef convexHullDataSets
        writeIORef convexHullDataSets (currentDataSet ++ [newDataSet])

        -- Print out the new list so the user can see it
        putStrLn "Available Data Sets:"
        putStrLn (pointListToCleanStr 1 (currentDataSet ++ [newDataSet]))
        putStr "\n"

    else if (menuSelection == "4") then do
        -- Run a specific algorithm on a specific dataset
        putStrLn "What Algorithm do you want?"
        putStrLn (displayAlgorithms algorithms)
        putStrLn "Enter Selection (as an integer): "
        algorithmSelection <- getLine

        putStrLn "What dataset do you want?"
        datasetList <- readIORef convexHullDataSets
        putStrLn (pointListToCleanStr 1 datasetList) -- needs nums
        putStrLn "Enter Selection (as an integer):"
        dataSelection <- getLine

        -- Print result and draw chart
        putStrLn "Result:\n-------"
        print (runAlgorithm datasetList algorithmSelection dataSelection)
        --drawChart 
        --    ((datasetList!!0) \\ (runAlgorithm datasetList algorithmSelection dataSelection)) -- All points not in the hull
        --    ((runAlgorithm datasetList algorithmSelection dataSelection) ++ head ((runAlgorithm datasetList algorithmSelection dataSelection)))  -- Points in the hull, with the head added to the end so that we draw a complete polygon
        --    "Result of Graham's Scan on Dataset\n" ++  -- Chart Title

        putStrLn "\nBenchmark:\n----------"
        Criterion.benchmark (Criterion.whnf (runAlgorithm datasetList algorithmSelection) dataSelection)



    else if (menuSelection == "5") then do
        -- Run all of the available algorithms on all the available datasets
        datasetList <- readIORef convexHullDataSets
        runAllAlgorithms datasetList
        putStrLn ""

    else if (menuSelection == "6") then do
        -- Exit
        putStrLn "Bye!"
        exitSuccess

    else
        -- Redo the main loop
        putStr "We don't recognize that option. Try again"

    -- Redo the main loop (resursively)
    exec convexHullDataSets -- https://stackoverflow.com/questions/9015318/what-to-use-instead-of-a-main-loop-in-haskell

-- Main Function
main :: IO ()
main = do -- figure out how do works and how to pass it in to things -JC

    -- A mutable IORef variable
    -- https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-IORef.html
    convexHullData <- newIORef datasets
    exec convexHullData
    
        -- putStrLn "Jarvis March (not working properly):"
        -- putStr "Result from data set 1: "
        -- print (runAlgorithm "1" "1")
        -- putStrLn "Timing: "
        -- Criterion.benchmark (Criterion.whnf (runAlgorithm "1") "1")
        -- putStr "Result from data set 2: "
        -- print (runAlgorithm "1" "2")
        -- putStrLn "Timing: "
        -- Criterion.benchmark (Criterion.whnf (runAlgorithm "1") "2")
        -- putStr "Result from data set 3: "
        -- print (runAlgorithm "1" "3")
        -- putStrLn "Timing: "
        -- Criterion.benchmark (Criterion.whnf (runAlgorithm "1") "3")
        -- putStr "\n"

        -- putStrLn "Graham's Scan:"
        -- putStr "Result from data set 1: "
        -- print (runAlgorithm "2" "1")
        -- putStrLn "Timing: "
        -- Criterion.benchmark (Criterion.whnf (runAlgorithm "2") "1")
        -- putStr "Result from data set 2: "
        -- print (runAlgorithm "2" "2")
        -- putStrLn "Timing: "
        -- Criterion.benchmark (Criterion.whnf (runAlgorithm "2") "2")
        -- putStr "Result from data set 3: "
        -- print (runAlgorithm "2" "3")
        -- putStrLn "Timing: "
        -- Criterion.benchmark (Criterion.whnf (runAlgorithm "2") "3")
        -- putStr "\n"

    -- else if (menuSelection == "2") then do        
    --     putStrLn "What Algorithm do you want?\n1. Jarvis March (not working properly)\n2. Graham's Scan\n"
    --     putStrLn "Enter Selection (as an integer): "
    --     algorithmSelection <- getChar
    --     putStr "\n"

        -- putStrLn "What precanned data set do you want?"
        -- putStr "1. "
        -- print exampleInput1
        -- putStr "2. "
        -- print exampleInput2
        -- putStr "3. "
        -- print exampleInput3
        -- putStrLn ""
        -- putStrLn "Enter Selection (as an integer): \n"
        -- dataSelection <- getLine

        -- putStr "Results: "
        -- print (runAlgorithm algorithmSelection dataSelection)
        -- putStrLn ""
        -- putStrLn "Timing Information: "
        -- Criterion.benchmark (Criterion.whnf (runAlgorithm algorithmSelection) dataSelection)