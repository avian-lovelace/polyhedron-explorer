module Midsphere
  ( getMidsphereRadius,
    getMidspherePolar,
  )
where

import Data.Map ((!))
import Pair
import Polyhedron
import Projection
import Space

getMidsphereRadius :: (Ord v, Ord f) => Polyhedron v f -> Double
getMidsphereRadius polyhedron = head edgeDistances
  where
    Polyhedron {vertexPoints} = polyhedron
    edges = getEdges polyhedron
    edgeDistances =
      [ distance zero midspherePoint
        | Edge (Pair v1 v2) _ <- edges,
          let p1 = vertexPoints ! v1,
          let p2 = vertexPoints ! v2,
          let midspherePoint = projectionOnLine p1 p2 zero
      ]

getMidspherePolar :: Double -> Point -> Point -> Point -> Point
getMidspherePolar midRadius p1 p2 p3 = vectorEndpoint zero poleVector
  where
    poleCenter = projectionOnPlane p1 p2 p3 zero
    poleCenterVector = differenceVector zero poleCenter
    poleDistance = norm poleCenterVector
    polarDistance = midRadius * midRadius / poleDistance
    poleVector = polarDistance /*/ normalize poleCenterVector