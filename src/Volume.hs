module Volume
  ( tetrahedronVolume,
    triangulate,
    pyramidVolume,
    polyhedronVolume,
    volumeNormalize,
  )
where

import qualified Data.Map as Map
import Polyhedron
import Space

tetrahedronVolume :: Point -> Point -> Point -> Double
tetrahedronVolume (Point (x1, y1, z1)) (Point (x2, y2, z2)) (Point (x3, y3, z3)) =
  abs $ ((x1 * y2 * z3) + (x2 * y3 * z1) + (x3 * y1 * z2) - (x3 * y2 * z1) - (x2 * y1 * z3) - (x1 * y3 * z2)) / 6

triangulate :: [Point] -> [(Point, Point, Point)]
triangulate [] = []
triangulate [_p1] = []
triangulate [_p1, _p2] = []
triangulate (p1 : p2 : p3 : ps) = (p1, p2, p3) : triangulate (p1 : p3 : ps)

pyramidVolume :: [Point] -> Double
pyramidVolume face = sum tetrahedronVolumes
  where
    triangles = triangulate face
    tetrahedronVolumes = fmap (\(p1, p2, p3) -> tetrahedronVolume p1 p2 p3) triangles

polyhedronVolume :: (Ord v) => Polyhedron v f -> Double
polyhedronVolume polyhedron = sum pyramidVolumes
  where
    facePoints = toFacePoints polyhedron
    pyramidVolumes = pyramidVolume <$> facePoints

volumeNormalize :: (Ord v) => Polyhedron v f -> Polyhedron v f
volumeNormalize p = Polyhedron {vertices = Map.map (elementWise (* sizeMultiplier)) vertices, faces}
  where
    (Polyhedron {vertices, faces}) = p
    volume = polyhedronVolume p
    sizeMultiplier = volume ** (-(1 / 3))