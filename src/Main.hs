module Main where

import Algorithms
import Interactive
import Criterion

main :: IO ()
main = do
    putStrLn "Welcome to the Convex Hull App\n---------------------------------------------------\n"
    putStrLn "What would you like to do?"
    putStrLn "    1. View some precanned runs of the algorithms I've got implemented"
    putStrLn "    2. Select an algorithm and a data set you want to run the selected algorithm on"
    putStrLn "    3. Exit\n"
    putStrLn "Enter Selection (as an integer): "

    menuSelection <- getLine
    putStr "\n"

    if (menuSelection == "1") then do
        putStrLn "Example Data Sets:"
        print exampleInput1
        print exampleInput2
        print exampleInput3
        putStr "\n"

        -- putStrLn "Jarvis March:"
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

        putStrLn "Graham's Scan:"
        putStr "Result from data set 1: "
        print (runAlgorithm "2" "1")
        putStrLn "Timing: "
        Criterion.benchmark (Criterion.whnf (runAlgorithm "2") "1")
        putStr "Result from data set 2: "
        print (runAlgorithm "2" "2")
        putStrLn "Timing: "
        Criterion.benchmark (Criterion.whnf (runAlgorithm "2") "2")
        putStr "Result from data set 3: "
        print (runAlgorithm "2" "3")
        putStrLn "Timing: "
        Criterion.benchmark (Criterion.whnf (runAlgorithm "2") "3")
        putStr "\n"

    else if (menuSelection == "2") then do
        putStrLn "What Algorithm do you want?\n1. Jarvis March\n2. Graham's Scan\n"
        putStrLn "Enter Selection (as an integer): "
        algorithmSelection <- getLine
        putStr "\n"

        putStrLn "What precanned data set do you want?"
        putStr "1. "
        print exampleInput1
        putStr "2. "
        print exampleInput2
        putStr "3. "
        print exampleInput3
        putStrLn ""
        putStrLn "Enter Selection (as an integer): \n"
        dataSelection <- getLine

        putStr "Results: "
        print (runAlgorithm algorithmSelection dataSelection)
        putStrLn ""
        putStrLn "Timing: "
        Criterion.benchmark (Criterion.whnf (runAlgorithm algorithmSelection) dataSelection)
    else
        putStrLn "Bye!"

