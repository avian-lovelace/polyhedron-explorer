module Lib
  ( main,
  )
where

import Data.Foldable (find, traverse_)
import Data.List (elemIndex, intercalate)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import GHC.Float (int2Double)

main :: IO ()
main = do
  let normalizedOctahedron = volumeNormalize octahedron
  printPolyhedron normalizedOctahedron
  print . polyhedronVolume $ normalizedOctahedron

newtype Point = Point (Double, Double, Double)

zero :: Point
zero = Point (0, 0, 0)

elementWise :: (Double -> Double) -> Point -> Point
elementWise f (Point (x, y, z)) = Point (f x, f y, f z)

elementWise' :: (Double -> Double -> Double) -> Point -> Point -> Point
elementWise' f (Point (x1, y1, z1)) (Point (x2, y2, z2)) = Point (f x1 x2, f y1 y2, f z1 z2)

triangulate :: [Point] -> [(Point, Point, Point)]
triangulate [] = []
triangulate [p1] = []
triangulate [p1, p2] = []
triangulate (p1 : p2 : p3 : ps) = (p1, p2, p3) : triangulate (p1 : p3 : ps)

tetrahedronVolume :: Point -> Point -> Point -> Double
tetrahedronVolume (Point (x1, y1, z1)) (Point (x2, y2, z2)) (Point (x3, y3, z3)) =
  abs $ ((x1 * y2 * z3) + (x2 * y3 * z1) + (x3 * y1 * z2) - (x3 * y2 * z1) - (x2 * y1 * z3) - (x1 * y3 * z2)) / 6

pyramidVolume :: [Point] -> Double
pyramidVolume face = sum tetrahedronVolumes
  where
    triangles = triangulate face
    tetrahedronVolumes = fmap (\(p1, p2, p3) -> tetrahedronVolume p1 p2 p3) triangles

instance Show Point where
  show (Point p) = show p

-- show (Point (x, y, z)) = show (getSign x, getSign y, getSign z)

data Polyhedron v f = Polyhedron
  { vertices :: Map v Point,
    faces :: Map f [v]
  }

generatePolyhedron :: (Ord v, Ord f) => [v] -> (v -> Point) -> [f] -> (f -> [v]) -> Polyhedron v f
generatePolyhedron vertexGenerators vgToPoint faceGenerators fgToFace =
  Polyhedron
    { vertices = Map.fromList [(g, vgToPoint g) | g <- vertexGenerators],
      faces = Map.fromList [(g, fgToFace g) | g <- faceGenerators]
    }

toFacePoints :: (Ord v) => Polyhedron v f -> [[Point]]
toFacePoints (Polyhedron {vertices, faces}) = [(vertices !) <$> face | (_, face) <- Map.toList faces]

printPolyhedron :: (Ord v) => Polyhedron v f -> IO ()
printPolyhedron polyhedron = do
  let Polyhedron {vertices, faces} = polyhedron
  let verticesListString = intercalate ", " [show vertex | (_, vertex) <- Map.toList vertices]
  putStrLn $ "Vertices: " ++ verticesListString

  putStrLn "Faces:"
  let facePoints = toFacePoints polyhedron
  let getFaceString face = intercalate ", " $ show <$> face
  let printFace face = putStrLn $ getFaceString face
  traverse_ printFace facePoints

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

data Axis = X | Y | Z deriving (Eq, Ord)

axes :: [Axis]
axes = [X, Y, Z]

unitAxis :: Axis -> Point
unitAxis X = Point (1, 0, 0)
unitAxis Y = Point (0, 1, 0)
unitAxis Z = Point (0, 0, 1)

insertInAxis :: Axis -> a -> (a, a) -> (a, a, a)
insertInAxis X r (s, t) = (r, s, t)
insertInAxis Y r (s, t) = (t, r, s)
insertInAxis Z r (s, t) = (s, t, r)

data Sign = Neg | Zero | Pos deriving (Eq, Ord)

instance Show Sign where
  show Pos = "+"
  show Zero = "0"
  show Neg = "-"

plusMinus :: [Sign]
plusMinus = [Pos, Neg]

getSign :: Double -> Sign
getSign x
  | x > 0 = Pos
  | x == 0 = Zero
  | otherwise = Neg

signMultiplier :: Sign -> Double
signMultiplier Pos = 1
signMultiplier Zero = 0
signMultiplier Neg = -1

timesSign :: Sign -> Point -> Point
timesSign Pos (Point (x, y, z)) = Point (x, y, z)
timesSign Zero _ = Point (0, 0, 0)
timesSign Neg (Point (x, y, z)) = Point (-x, -y, -z)

octahedron :: Polyhedron (Axis, Sign) (Sign, Sign, Sign)
octahedron = generatePolyhedron vertexGenerators vgToPoint faceGenerators fgToFace
  where
    vertexGenerators = [(axis, sign) | axis <- axes, sign <- plusMinus]
    vgToPoint (axis, sign) = timesSign sign (unitAxis axis)
    faceGenerators = [(xSign, ySign, zSign) | xSign <- plusMinus, ySign <- plusMinus, zSign <- plusMinus]
    fgToFace (xSign, ySign, zSign) = [(X, xSign), (Y, ySign), (Z, zSign)]

cube :: Polyhedron (Sign, Sign, Sign) (Axis, Sign)
cube = generatePolyhedron vertexGenerators vgToPoint faceGenerators fgToFace
  where
    vertexGenerators = [(xSign, ySign, zSign) | xSign <- plusMinus, ySign <- plusMinus, zSign <- plusMinus]
    vgToPoint (xSign, ySign, zSign) = Point (signMultiplier xSign, signMultiplier ySign, signMultiplier zSign)
    faceGenerators = [(axis, sign) | axis <- axes, sign <- plusMinus]
    faceGeneratorTemplate = [(Pos, Pos), (Pos, Neg), (Neg, Neg), (Neg, Pos)]
    fgToFace (axis, sign) = insertInAxis axis sign <$> faceGeneratorTemplate

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