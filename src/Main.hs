module Main where

import Algorithms
import Interactive

jarvisMarchResults = runAlgorithm "1"
grahamsScanResults = runAlgorithm "2"

main :: IO ()
main = do
    putStrLn "Welcome to the Convex Hull App\n---------------------------------------------------\n"
    putStrLn "What would you like to do?"
    putStrLn "    1. View some precanned runs of the algorithms I've got implemented"
    putStrLn "    2. Enter some input and select the algorithm you want to run"
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

        putStrLn "Jarvis March:"
        putStr "Run 1: "
        print (runAlgorithm "1" "1")
        putStr "Run 2: "
        print (runAlgorithm "1" "2")
        putStr "Run 3: "
        print (runAlgorithm "1" "3")
        putStr "\n"

        putStrLn "Graham's Scan:"
        putStr "Run 1: "
        print (runAlgorithm "2" "1")
        putStr "Run 2: "
        print (runAlgorithm "2" "2")
        putStr "Run 3: "
        print (runAlgorithm "2" "3")
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
        putStrLn "Enter Selection (as an integer): \n"
        dataSelection <- getLine
        putStr "\n"

        putStr "Results: "
        print (runAlgorithm algorithmSelection dataSelection)

        --putStr "Enter the points you want convex hulled, in the form:"
    else
        putStrLn "Bye!"

