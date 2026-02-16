module Svg (getSvgString) where

import Space

borderMargin :: Double
borderMargin = 0.1

styleString :: String
styleString = "fill:none; stroke:black; stroke-width:0.05"

getSvgString :: [Polygon v] -> String
getSvgString polygons = svgStartTag ++ polygonsString ++ svgEndTag
  where
    svgStartTag = "<svg " ++ getViewBoxString polygons ++ " xmlns=\"http://www.w3.org/2000/svg\">"
    svgEndTag = "</svg>"
    polygonsString = concatMap getPolygonString polygons

getViewBoxString :: [Polygon v] -> String
getViewBoxString polygons = "viewBox=\"" ++ unwords [show xStart, show yStart, show xSpan, show ySpan] ++ "\""
  where
    xValues = [x | Polygon polygon <- polygons, (_, Point2D (x, _)) <- polygon]
    yValues = [y | Polygon polygon <- polygons, (_, Point2D (_, y)) <- polygon]
    xMin = minimum xValues
    xMax = maximum xValues
    yMin = minimum yValues
    yMax = maximum yValues
    xRange = xMax - xMin
    yRange = yMax - yMin
    xStart = xMin - borderMargin * xRange
    xSpan = xRange + 2 * borderMargin * xRange
    yStart = yMin - borderMargin * yRange
    ySpan = yRange + 2 * borderMargin * yRange

getPolygonString :: Polygon v -> String
getPolygonString (Polygon polygon) = "<polygon points=\"" ++ pointsString ++ "\" style=\"" ++ styleString ++ "\"/>"
  where
    pointsString = unwords $ [show x ++ "," ++ show y | (_, Point2D (x, y)) <- polygon]