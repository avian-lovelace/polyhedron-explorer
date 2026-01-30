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
  let Polyhedron {vertices} = polyhedron
  let verticesListString = intercalate ", " [show vertex | (_, vertex) <- Map.toList vertices]
  putStrLn $ "Vertices: " ++ verticesListString

  putStrLn "Faces:"
  let facePoints = toFacePoints polyhedron
  let getFaceString face = intercalate ", " $ show <$> face
  let printFace face = putStrLn $ getFaceString face
  traverse_ printFace facePoints