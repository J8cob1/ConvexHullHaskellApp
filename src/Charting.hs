-- A place to put charting code
-- Worked off examples from: 
--   https://github.com/timbod7/haskell-chart/wiki/example-10 and 
--   https://github.com/timbod7/haskell-chart/wiki/example-8

module Charting where

-- Import Graphics Library
import Algorithms
import Data.List
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

-- Takes data sets and a title and renders a graph to a file with a specific name
-- Not sure what the type def should be because I don't know what the return type is
drawChart :: [Point2D] -> [Point2D] -> String -> String -> IO ()
drawChart allPoints hullPoints title fileName = 
    -- The value after "def" specifies the name of the file. It seems that I can only get it to draw to svg photos
    toFile def (fileName ++ ".svg") $ do
        -- Set Graph Graphics Options
        setColors (map opaque [blue,green,green])
        setShapes ([PointShapeCircle, (PointShapePolygon 3 True)])

        -- Set graph title
        layout_title .= title

        -- Draw points not on hull
        plot $ points "Non-hull Points" $ allPoints \\ hullPoints -- Looked at https://stackoverflow.com/questions/940382/what-is-the-difference-between-dot-and-dollar-sign, though I was already made aware of what the dollar sign did

        -- Draw points on hull and a line connecting them (to form the hull)
        plot $ points "Convex Hull Points" hullPoints 
        plot (line "Convex Hull Line" [hullPoints ++ [(head hullPoints)]]) -- we add the point at the beginning of the hull to the end so a complete shape is rendered