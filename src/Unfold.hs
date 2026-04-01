module Unfold
  ( unfoldVertex,
    unfoldVertexWhere,
  )
where

import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import Polyhedron
import Projection
import Space

unfoldVertex :: (Ord v, Ord f) => Polyhedron v f -> v -> [Polygon v]
unfoldVertex polygon sharedVertex = transformedFaces
  where
    pointFaces = getPointFaces polygon
    sharedVertexFaces = vertices polygon ! sharedVertex
    targetFaces = [pointFaces ! face | face <- sharedVertexFaces]
    projectedFaces = faceTo2D . vectorizeFace <$> targetFaces
    (firstFace : restFaces) = projectedFaces
    Polygon firstFaceVertices = firstFace
    (Just (_, sharedVertexPoint)) = List.find (\(v, _) -> v == sharedVertex) firstFaceVertices
    -- For the first face, all we need to do is tranlate it such that sharedVertexPoint is at the origin
    transformedFirstFace = applyTransform (translateToZero sharedVertexPoint) firstFace
    -- For all the remaining faces, we want to apply a geometric transformation such that the shared edge between it and
    -- the previous face are aligned and the two faces are on opposite sides of the shared edge.
    transformNextFace transformedFaces nextFace = transformedNextFace' : transformedFaces
      where
        Polygon nextFaceVertices = nextFace
        targetFace = head transformedFaces
        Polygon targetFaceVertices = head transformedFaces
        nextFaceVerticesMap = Map.fromList nextFaceVertices
        targetFaceVerticesMap = Map.fromList targetFaceVertices
        sharedVerticesMap = Map.intersectionWith (,) nextFaceVerticesMap targetFaceVerticesMap
        -- Because two adjacent faces have two vertices in common, this list should have exactly two elements
        [(v1, pair1), (_, pair2)] = Map.toList sharedVerticesMap
        -- We can ignore targetPoint1 because we know it should be (0, 0)
        ((basePoint1, _), (basePoint2, targetPoint2)) = if v1 == sharedVertex then (pair1, pair2) else (pair2, pair1)
        transform = matchSegments basePoint1 basePoint2 targetPoint2
        -- Rotate and dilate nextFace such that the shared edge with targetFace aligns
        transformedNextFace = applyTransform transform nextFace
        -- This final step figures out whether transformedNextFace should to be reflected to make sure they are on
        -- opposite sides of the shared edge and not overlapping. The strategy is to look at transformedNextFace and
        -- its reflection and choose the one whose center is further from the center of targetFace.
        transformedNextFaceReflected = applyTransform (reflectOverSegment targetPoint2) transformedNextFace
        targetFaceCenter = polygonCenter targetFace
        transformedNextFaceCenter = polygonCenter transformedNextFace
        transformedNextFaceReflectedCenter = polygonCenter transformedNextFaceReflected
        distance1 = distance2D targetFaceCenter transformedNextFaceCenter
        distance2 = distance2D targetFaceCenter transformedNextFaceReflectedCenter
        transformedNextFace' = if distance1 > distance2 then transformedNextFace else transformedNextFaceReflected
    transformedFaces = List.foldl' transformNextFace [transformedFirstFace] restFaces

unfoldVertexWhere :: (Ord v, Ord f) => Polyhedron v f -> (v -> Bool) -> [Polygon v]
unfoldVertexWhere polygon vertexFilter = unfoldVertex polygon matchingVertex
  where
    verticesList = Map.keys . vertices $ polygon
    Just matchingVertex = List.find vertexFilter verticesList