module Operators
  ( dualPolyhedron,
    amboPolyhedron,
  )
where

import Data.Map ((!))
import qualified Data.Map as Map
import GHC.Float (int2Double)
import Pair
import Polyhedron
import Space

dualPolyhedron :: (Ord v) => Polyhedron v f -> Polyhedron f v
dualPolyhedron (Polyhedron {vertexPoints, vertices, faces}) = Polyhedron {vertexPoints = faceCenters, vertices = faces, faces = vertices}
  where
    faceCenter vertexOrder = elementWise (/ (int2Double $ length vertexOrder)) $ foldr (elementWise' (+) . (vertexPoints !)) zero vertexOrder
    faceCenters = Map.map faceCenter faces

amboPolyhedron :: (Ord v, Ord f) => Polyhedron v f -> Polyhedron (Edge v f) (Either v f)
amboPolyhedron polyhedron = Polyhedron {vertexPoints = vertexPoints', vertices = vertices', faces = faces'}
  where
    (Polyhedron {vertexPoints, vertices, faces}) = polyhedron
    edges = getEdges polyhedron
    facePairToEdgeMap = Map.fromList [(fPair, edge) | edge <- edges, let Edge _ fPair = edge]
    vertexPairToEdgeMap = Map.fromList [(vPair, edge) | edge <- edges, let Edge vPair _ = edge]
    vertexPoints' =
      Map.fromList
        [ (edge, edgeCenter)
          | edge <- edges,
            let Edge (Pair v1 v2) _ = edge,
            let p1 = vertexPoints ! v1,
            let p2 = vertexPoints ! v2,
            let edgeCenter = elementWise (/ 2) $ elementWise' (+) p1 p2
        ]
    vertices' =
      Map.fromList
        [ (edge, [Left v1, Right f1, Left v2, Right f2])
          | edge <- edges,
            let Edge (Pair v1 v2) (Pair f1 f2) = edge
        ]
    vertexFaces =
      [ (Left vertex, incidentEdges)
        | (vertex, orderedFaces) <- Map.toList vertices,
          let facePairs = getAdjacentPairs orderedFaces,
          let incidentEdges = [facePairToEdgeMap ! fPair | fPair <- facePairs]
      ]
    faceFaces =
      [ (Right face, incidentEdges)
        | (face, orderedVertices) <- Map.toList faces,
          let vertexPairs = getAdjacentPairs orderedVertices,
          let incidentEdges = [vertexPairToEdgeMap ! vPair | vPair <- vertexPairs]
      ]
    faces' = Map.fromList $ vertexFaces ++ faceFaces
