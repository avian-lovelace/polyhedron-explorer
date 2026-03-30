module Lib
  ( main,
  )
where

import qualified Data.List as List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Operators
import Platonic
import Polyhedron
import Projection
import Space
import Svg

main :: IO ()
main = do
  let poly = dualPolyhedron . amboPolyhedron $ icosahedron
  let pointFaces = getPointFaces poly
  let (sharedVertex, sharedVertexFaces) = head $ Map.toList (vertices poly)
  let targetFaces = [pointFaces ! face | face <- sharedVertexFaces]
  let projectedFaces = faceTo2D . vectorizeFace <$> targetFaces
  let (firstFace : restFaces) = projectedFaces
  let Polygon firstFaceVertices = firstFace
  let (Just (_, sharedVertexPoint)) = List.find (\(v, _) -> v == sharedVertex) firstFaceVertices
  -- For the first face, all we need to do is tranlate it such that sharedVertexPoint is at the origin
  let transformedFirstFace = applyTransform (translateToZero sharedVertexPoint) firstFace
  -- For all the remaining faces, we want to apply a geometric transformation such that the shared edge between it and
  -- the previous face are aligned and the two faces are on opposite sides of the shared edge.
  let transformNextFace transformedFaces nextFace = transformedNextFace' : transformedFaces
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
  let transformedFaces = List.foldl' transformNextFace [transformedFirstFace] restFaces
  let svgString = getSvgString transformedFaces
  let filePath = "./out/" ++ "output.svg"
  writeFile filePath svgString

-- case validatePolyhedron foo of
--   Left errorMessage -> putStrLn errorMessage
--   Right () -> do
--     displaySolid "output.svg" foo