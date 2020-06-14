# ConvexHullHaskellApp

This application, developed as a part of a course at Portland State University, with continued development in a second, allows you to run Convex Hull Algorithms on various datasets. 

## Running
stack install
stack run

## Current Features
- Allows you to run three algorithms on convex hull datasets via a terminal user interface
- Graphs the results (by generating .svg images with the convex hull outlined)
- Allows you to enter and randomly generate your own custom datasets using some parameters that you can specify

## Problems
- The Graham's Scan and QuickHull algorithms don't seem to work right (if you generate a couple of large datasets with x and y values of great range and decent size you might see)