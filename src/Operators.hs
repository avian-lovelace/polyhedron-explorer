module Operators
  ( dualPolyhedron,
  )
where

import Data.Foldable (find)
import Data.List (elemIndex)
import Data.Map ((!))
import qualified Data.Map as Map
import GHC.Float (int2Double)
import Polyhedron
import Space

dualPolyhedron :: (Ord v, Ord f) => Polyhedron v f -> Polyhedron f v
dualPolyhedron (Polyhedron {vertices, faces}) = Polyhedron {vertices = dualVertices, faces = dualFaces}
  where
    faceCenter face = elementWise (/ (int2Double $ length face)) $ foldr (elementWise' (+) . (vertices !)) zero face
    dualVertices = Map.map faceCenter faces
    incidentFaces vertex = Map.filter (\face -> vertex `elem` face) faces
    adjacentVertices vertex face = (face !! adjacentIndexMinus, face !! adjacentIndexPlus)
      where
        Just vertexIndex = elemIndex vertex face
        faceLength = length face
        adjacentIndexPlus = (vertexIndex + 1) `mod` faceLength
        adjacentIndexMinus = (vertexIndex + faceLength - 1) `mod` faceLength
    getDualFace adjacentVerticesMap = getDualFace' firstVertex [firstFace] (Map.delete firstFace adjacentVerticesMap)
      where
        adjacentVerticesList = Map.toList adjacentVerticesMap
        (firstFace, (lastVertex, firstVertex)) = head adjacentVerticesList
        getDualFace' currentVertex usedFaces unusedFaces =
          if currentVertex == lastVertex
            then usedFaces
            else getDualFace' nextVertex (nextFace : usedFaces) (Map.delete nextFace unusedFaces)
          where
            Just (nextFace, nextVertices) = find (\(_, (v1, v2)) -> v1 == currentVertex || v2 == currentVertex) (Map.toList unusedFaces)
            nextVertex = if fst nextVertices == currentVertex then snd nextVertices else fst nextVertices
    dualFaces = Map.mapWithKey (\vertex _ -> getDualFace $ Map.map (adjacentVertices vertex) (incidentFaces vertex)) vertices