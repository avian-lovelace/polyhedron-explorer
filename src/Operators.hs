module Operators
  ( dualPolyhedron,
  )
where

import Data.Map ((!))
import qualified Data.Map as Map
import GHC.Float (int2Double)
import Polyhedron
import Space

dualPolyhedron :: (Ord v) => Polyhedron v f -> Polyhedron f v
dualPolyhedron (Polyhedron {vertexPoints, vertices, faces}) = Polyhedron {vertexPoints = faceCenters, vertices = faces, faces = vertices}
  where
    faceCenter vertexOrder = elementWise (/ (int2Double $ length vertexOrder)) $ foldr (elementWise' (+) . (vertexPoints !)) zero vertexOrder
    faceCenters = Map.map faceCenter faces