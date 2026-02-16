module Projection
  ( vectorizeFace,
    faceTo2D,
    projectFace,
    offsetNormalBasis,
  )
where

import GHC.Float (int2Double)
import Polyhedron
import Space

newtype Vector = Vector (Double, Double, Double)

(/+/) :: Vector -> Vector -> Vector
(Vector (x1, y1, z1)) /+/ (Vector (x2, y2, z2)) = Vector (x1 + x2, y1 + y2, z1 + z2)

(/-/) :: Vector -> Vector -> Vector
(Vector (x1, y1, z1)) /-/ (Vector (x2, y2, z2)) = Vector (x1 - x2, y1 - y2, z1 - z2)

(/*/) :: Double -> Vector -> Vector
c /*/ (Vector (x, y, z)) = Vector (c * x, c * y, c * z)

differenceVector :: Point -> Point -> Vector
differenceVector (Point (x1, y1, z1)) (Point (x2, y2, z2)) = Vector (x2 - x1, y2 - y1, z2 - z1)

dotProduct :: Vector -> Vector -> Double
dotProduct (Vector (x1, y1, z1)) (Vector (x2, y2, z2)) = x1 * x2 + y1 * y2 + z1 * z2

crossProduct :: Vector -> Vector -> Vector
crossProduct (Vector (x1, y1, z1)) (Vector (x2, y2, z2)) = Vector (y1 * z2 - y2 * z1, z1 * x2 - z2 * x1, x1 * y2 - x2 * y1)

orthonormalBasis :: Vector -> Vector -> (Vector, Vector)
orthonormalBasis v1 v2 = (n1, n2)
  where
    n1 = (1 / sqrt (dotProduct v1 v1)) /*/ v1
    n2' = v2 /-/ (dotProduct n1 v2 /*/ n1)
    n2 = (1 / sqrt (dotProduct n2' n2')) /*/ n2'

type VectorFace v = [(v, Vector)]

vectorizeFace :: Face v -> VectorFace v
vectorizeFace face = [(vertex, differenceVector faceCenter vertexPoint) | (vertex, vertexPoint) <- face]
  where
    vertexPoints = [point | (_, point) <- face]
    faceCenter = elementWise (/ (int2Double $ length vertexPoints)) $ foldr (elementWise' (+)) zero vertexPoints

faceTo2D :: VectorFace v -> Polygon v
faceTo2D face = Polygon [(vertex, Point2D (dotProduct vector n1, dotProduct vector n2)) | (vertex, vector) <- face]
  where
    (_, v1) : (_, v2) : _ = face
    (n1, n2) = orthonormalBasis v1 v2

projectFace :: Vector -> Vector -> Face v -> Polygon v
projectFace n1 n2 face =
  Polygon
    [ (vertex, Point2D (dotProduct vector n1, dotProduct vector n2))
      | (vertex, point) <- face,
        let vector = toOriginVector point
    ]
  where
    toOriginVector (Point (x, y, z)) = Vector (x, y, z)

offsetNormalBasis :: (Vector, Vector)
offsetNormalBasis = orthonormalBasis (Vector (1, 3, 3)) (Vector (4, 2, 1))