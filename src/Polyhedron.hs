module Polyhedron
  ( Polyhedron (..),
    generatePolyhedron,
    toFacePoints,
    printPolyhedron,
  )
where

import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Map (Map, (!))
import qualified Data.Map as Map
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

-- getAdjacentPairs :: [a] -> [(a, a)]
-- getAdjacentPairs xs = getAdjacentPairs' [] xs
--   where
--     firstElem = head xs
--     getAdjacentPairs' currentPairs (nextElem : nextNextElem : restElems) =
--       getAdjacentPairs' ((nextElem, nextNextElem) : currentPairs) (nextNextElem : restElems)
--     getAdjacentPairs' currentPairs [lastElem] = (lastElem, firstElem) : currentPairs
--     getAdjacentPairs' _currentPairs [] = undefined

-- getAdjacentOrderedPairs :: (Ord a) => [a] -> [(a, a)]
-- getAdjacentOrderedPairs xs = [if x1 < x2 then (x1, x2) else (x2, x1) | (x1, x2) <- getAdjacentPairs xs]

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