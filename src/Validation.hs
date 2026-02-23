module Validation
  ( validatePolyhedron,
  )
where

import qualified Data.Set as Set
import Polyhedron

validatePolyhedron :: (Ord v, Ord f) => Polyhedron v f -> Either String ()
validatePolyhedron polyhedron = do
  runCheck "Face orders and vertex orders do not match" faceAndVertexOrdersMatch
  where
    runCheck errorMessage check = if check polyhedron then Right () else Left errorMessage

faceAndVertexOrdersMatch :: (Ord v, Ord f) => Polyhedron v f -> Bool
faceAndVertexOrdersMatch polyhedron = edges == edges'
  where
    edges = Set.fromList . getEdges $ polyhedron
    edges' = Set.fromList . getEdges' $ polyhedron
