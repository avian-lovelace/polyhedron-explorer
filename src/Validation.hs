module Validation
  ( validatePolyhedron,
  )
where

import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Polyhedron
import Projection
import Space

validatePolyhedron :: (Ord v, Ord f) => Polyhedron v f -> Either String ()
validatePolyhedron polyhedron = do
  runCheck "Face orders and vertex orders do not match" faceAndVertexOrdersMatch
  runCheck "Face vertices are not coplanar" facesAreCoplanar
  where
    runCheck errorMessage check = if check polyhedron then Right () else Left errorMessage

faceAndVertexOrdersMatch :: (Ord v, Ord f) => Polyhedron v f -> Bool
faceAndVertexOrdersMatch polyhedron = edges == edges'
  where
    edges = Set.fromList . getEdges $ polyhedron
    edges' = Set.fromList . getEdges' $ polyhedron

facesAreCoplanar :: (Ord v) => Polyhedron v f -> Bool
facesAreCoplanar polyhedron = all verticesAreCoplanar faceVertesOrders
  where
    (Polyhedron {vertexPoints, faces}) = polyhedron
    faceVertesOrders = Map.elems faces
    verticesAreCoplanar (v1 : v2 : v3 : vs) = all vertexIsCoplanar vs
      where
        p1 = vertexPoints ! v1
        p2 = vertexPoints ! v2
        p3 = vertexPoints ! v3
        vertexIsCoplanar v = distance p p' / distance p zero < errorMargin
          where
            p = vertexPoints ! v
            p' = projectionOnPlane p1 p2 p3 p
            errorMargin = 0.001