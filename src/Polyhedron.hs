module Polyhedron
  ( Polyhedron (..),
    generatePolyhedron,
    toFacePoints,
    printPolyhedron,
    rescale,
    Edge (..),
    getEdges,
    getEdges',
    PointFace,
    getPointFaces,
  )
where

import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Pair
import Space

data Polyhedron v f = Polyhedron
  { vertexPoints :: Map v Point,
    vertices :: Map v [f],
    faces :: Map f [v]
  }

generatePolyhedron :: (Ord v, Ord f) => [v] -> (v -> Point) -> (v -> [f]) -> [f] -> (f -> [v]) -> Polyhedron v f
generatePolyhedron vertexGenerators vgToPoint vgToFaceOrder faceGenerators fgToVertexOrder =
  Polyhedron
    { vertexPoints = Map.fromList [(g, vgToPoint g) | g <- vertexGenerators],
      vertices = Map.fromList [(g, vgToFaceOrder g) | g <- vertexGenerators],
      faces = Map.fromList [(g, fgToVertexOrder g) | g <- faceGenerators]
    }

toFacePoints :: (Ord v) => Polyhedron v f -> [[Point]]
toFacePoints (Polyhedron {vertexPoints, faces}) = [(vertexPoints !) <$> face | (_, face) <- Map.toList faces]

printPolyhedron :: (Ord v) => Polyhedron v f -> IO ()
printPolyhedron polyhedron = do
  let Polyhedron {vertexPoints} = polyhedron
  let verticesListString = intercalate ", " [show vertex | (_, vertex) <- Map.toList vertexPoints]
  putStrLn $ "Vertices: " ++ verticesListString

  putStrLn "Faces:"
  let facePoints = toFacePoints polyhedron
  let getFaceString face = intercalate ", " $ show <$> face
  let printFace face = putStrLn $ getFaceString face
  traverse_ printFace facePoints

rescale :: Double -> Polyhedron v f -> Polyhedron v f
rescale sizeMultiplier (Polyhedron {vertexPoints, vertices, faces}) =
  Polyhedron {vertexPoints = Map.map (elementWise (* sizeMultiplier)) vertexPoints, vertices, faces}

data Edge v f = Edge (Pair v) (Pair f)
  deriving (Ord, Eq, Show)

getEdges :: (Ord v, Ord f) => Polyhedron v f -> [Edge v f]
getEdges (Polyhedron {faces}) = edgeList
  where
    faceEdgeList = [(vertexPair, [face]) | (face, orderedVertices) <- Map.toList faces, vertexPair <- getAdjacentPairs orderedVertices]
    edgeToFacesMap = Map.fromListWith (++) faceEdgeList
    edgeList = [Edge vPair (pairFromList fPair) | (vPair, fPair) <- Map.toList edgeToFacesMap]

{- Get the edge information using the face orders around each vertex rather than using the vertex orders around each
  face. If the polyhedron is defined correctly, getEdges' should give the same edges as getEdges, but possible in a
  different order. -}
getEdges' :: (Ord v, Ord f) => Polyhedron v f -> [Edge v f]
getEdges' (Polyhedron {vertices}) = edgeList
  where
    vertexEdgeList = [(facePair, [vertex]) | (vertex, orderedFaces) <- Map.toList vertices, facePair <- getAdjacentPairs orderedFaces]
    edgeToVerticesMap = Map.fromListWith (++) vertexEdgeList
    edgeList = [Edge (pairFromList vPair) fPair | (fPair, vPair) <- Map.toList edgeToVerticesMap]

type PointFace v = [(v, Point)]

getPointFaces :: (Ord v) => Polyhedron v f -> Map f (PointFace v)
getPointFaces polyhedron = Map.map toFace faces
  where
    Polyhedron {vertexPoints, faces} = polyhedron
    toFace vertexOrder = [(v, vertexPoints ! v) | v <- vertexOrder]