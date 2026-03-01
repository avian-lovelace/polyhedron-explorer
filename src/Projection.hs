module Projection
  ( differenceVector,
    distance,
    norm,
    normalize,
    (/*/),
    vectorEndpoint,
    vectorizeFace,
    faceTo2D,
    isPositivelyOriented,
    projectFace,
    offsetNormalBasis,
    projectionOnLine,
    projectionOnPlane,
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

vectorEndpoint :: Point -> Vector -> Point
vectorEndpoint (Point (x1, y1, z1)) (Vector (x2, y2, z2)) = Point (x1 + x2, y1 + y2, z1 + z2)

dotProduct :: Vector -> Vector -> Double
dotProduct (Vector (x1, y1, z1)) (Vector (x2, y2, z2)) = x1 * x2 + y1 * y2 + z1 * z2

crossProduct :: Vector -> Vector -> Vector
crossProduct (Vector (x1, y1, z1)) (Vector (x2, y2, z2)) = Vector (y1 * z2 - y2 * z1, z1 * x2 - z2 * x1, x1 * y2 - x2 * y1)

norm :: Vector -> Double
norm v = sqrt (dotProduct v v)

normalize :: Vector -> Vector
normalize v = (1 / norm v) /*/ v

distance :: Point -> Point -> Double
distance p1 p2 = norm $ differenceVector p1 p2

orthonormalBasis :: Vector -> Vector -> (Vector, Vector)
orthonormalBasis v1 v2 = (n1, n2)
  where
    n1 = normalize v1
    n2' = v2 /-/ (dotProduct n1 v2 /*/ n1)
    n2 = normalize n2'

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

isPositivelyOriented :: Vector -> Vector -> Face v -> Bool
isPositivelyOriented n1 n2 face = dotProduct n3 faceCenterVector > 0
  where
    n3 = crossProduct n1 n2
    vertexPoints = [point | (_, point) <- face]
    faceCenter = elementWise (/ (int2Double $ length vertexPoints)) $ foldr (elementWise' (+)) zero vertexPoints
    faceCenterVector = differenceVector zero faceCenter

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

projectionOnLine :: Point -> Point -> Point -> Point
projectionOnLine l1 l2 p = vectorEndpoint p orthogonalComponent
  where
    lineVector = normalize $ differenceVector l2 l1
    pVector = differenceVector p l2
    parallelComponent = dotProduct lineVector pVector /*/ lineVector
    orthogonalComponent = pVector /-/ parallelComponent

projectionOnPlane :: Point -> Point -> Point -> Point -> Point
projectionOnPlane s1 s2 s3 p = vectorEndpoint p orthogonalComponent
  where
    plane1 = differenceVector s2 s1
    plane2 = differenceVector s3 s1
    (n1, n2) = orthonormalBasis plane1 plane2
    pVector = differenceVector p s1
    parallelComponent1 = dotProduct n1 pVector /*/ n1
    parallelComponent2 = dotProduct n2 pVector /*/ n2
    orthogonalComponent = pVector /-/ parallelComponent1 /-/ parallelComponent2