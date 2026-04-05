module Volume
  ( tetrahedronVolume,
    triangulate,
    pyramidVolume,
    polyhedronVolume,
    volumeNormalizeIcosahedral,
    volumeNormalizeOctahedral,
  )
where

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

volumeNormalizeIcosahedral :: (Ord v) => Polyhedron v f -> Polyhedron v f
volumeNormalizeIcosahedral p = rescale sizeMultiplier p
  where
    volume = polyhedronVolume p
    sizeMultiplier = (standardIcosahedralVolumePxCubed / volume) ** (1 / 3)

volumeNormalizeOctahedral :: (Ord v) => Polyhedron v f -> Polyhedron v f
volumeNormalizeOctahedral p = rescale sizeMultiplier p
  where
    volume = polyhedronVolume p
    sizeMultiplier = (standardOctahedralVolumePxCubed / volume) ** (1 / 3)

pxPerInch :: Double
pxPerInch = 96

standardIcosahedralDiameterInches :: Double
standardIcosahedralDiameterInches = 4.5

standardIcosahedralVolumePxCubed :: Double
standardIcosahedralVolumePxCubed = (4 / 3) * pi * (standardIcosahedralDiameterInches * pxPerInch / 2) ** 3

standardOctahedralDiameterInches :: Double
standardOctahedralDiameterInches = 3

standardOctahedralVolumePxCubed :: Double
standardOctahedralVolumePxCubed = (4 / 3) * pi * (standardOctahedralDiameterInches * pxPerInch / 2) ** 3