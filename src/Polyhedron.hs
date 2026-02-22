module Polyhedron
  ( Polyhedron (..),
    generatePolyhedron,
    toFacePoints,
    printPolyhedron,
    Edge (..),
    getEdges,
    getEdges',
    Face,
    getFaces,
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

-- getAdjacentElements :: (Eq a) => a -> [a] -> Maybe (a, a)
-- getAdjacentElements center xs = do
--   centerIndex <- elemIndex center xs
--   let xsLength = length xs
--   let prevIndex = (centerIndex + xsLength - 1) `mod` xsLength
--   let nextIndex = (centerIndex + 1) `mod` xsLength
--   return (xs !! prevIndex, xs !! nextIndex)

data Edge v f = Edge (Pair v) (Pair f)
  deriving (Ord, Eq, Show)

getEdges :: (Ord v, Ord f) => Polyhedron v f -> [Edge v f]
getEdges (Polyhedron {faces}) = edgeList
  where
    faceEdgeList = [(vertexPair, [face]) | (face, orderedVertices) <- Map.toList faces, vertexPair <- getAdjacentPairs orderedVertices]
    edgeToFacesMap = Map.fromListWith (++) faceEdgeList
    edgeList = [Edge vPair (pairFromList fPair) | (vPair, fPair) <- Map.toList edgeToFacesMap]

getEdges' :: (Ord v, Ord f) => Polyhedron v f -> [Edge v f]
getEdges' (Polyhedron {vertices}) = edgeList
  where
    vertexEdgeList = [(facePair, [vertex]) | (vertex, orderedFaces) <- Map.toList vertices, facePair <- getAdjacentPairs orderedFaces]
    edgeToVerticesMap = Map.fromListWith (++) vertexEdgeList
    edgeList = [Edge (pairFromList vPair) fPair | (fPair, vPair) <- Map.toList edgeToVerticesMap]

-- invertMap :: (Ord a, Ord b) => Map a [b] -> Map b [a]
-- invertMap = undefined

-- Idea: Each edge should have exactly two vertices as endpoints and two incident faces. You can calculate this via the
-- vertex orders of each face and via the face orders around each vertex. If the polyhedron is valid, these two
-- calculations should lead to the same result.
-- Also, if you want each face/vertex order to be oriented the same way (let's say counterclockwise), you can check that
-- the vertex-vertex pair and face-face pars occur in opposite orders in their ocurrences in the face/vertex orders.

-- isValidPolyhedron :: (Ord v, Ord f) => Polyhedron v f -> Bool
-- isValidPolyhedron Polyhedron {vertices, faces} = undefined
--   where
--     getAdjacentVertices vertex face = getAdjacentElements vertex $ faces ! face
--     haveSharedVertex (v1, v2) (v1', v2') = v1 == v1' || v1 == v2' || v2 == v1' || v2 == v2'

-- pairsToListMap :: (Ord k) => [(k, v)] -> Map k [v]
-- pairsToListMap pairs = Map.fromListWith (++) [(k, [v]) | (k, v) <- pairs]

-- listMapToPairs :: Map k [v] -> [(k, v)]
-- listMapToPairs listMap = [(k, v) | (k, vs) <- Map.toList listMap, v <- vs]

type Face v = [(v, Point)]

getFaces :: (Ord v) => Polyhedron v f -> [Face v]
getFaces polyhedron = [toFace vertexOrder | (_, vertexOrder) <- Map.toList faces]
  where
    Polyhedron {vertexPoints, faces} = polyhedron
    toFace vertexOrder = [(v, vertexPoints ! v) | v <- vertexOrder]