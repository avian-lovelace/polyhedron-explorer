module Lib
  ( main,
  )
where

import Data.Foldable (traverse_)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Operators
import Platonic
import Polyhedron
import Projection
import Svg
import Volume

main :: IO ()
main = do
  -- let Polyhedron {faces, vertices} = dodecahedron
  -- printMap faces
  -- putStrLn ""
  -- printMap vertices

  -- let edges = getEdges dodecahedron
  -- let edges' = getEdges' dodecahedron
  -- let edgeSet = Set.fromList edges
  -- let edgeSet' = Set.fromList edges'
  -- print . length $ Set.difference edgeSet edgeSet'

  let icosadodecahedron = amboPolyhedron dodecahedron
  -- let triacontahedron = dualPolyhedron icosadodecahedron
  let faces = getFaces icosadodecahedron
  let (n1, n2) = offsetNormalBasis
  let faces2d = [projectFace n1 n2 f | f <- faces]
  -- let getFaceString face = intercalate ", " $ show . snd <$> face
  let svgString = getSvgString faces2d
  writeFile "./out/output.svg" svgString

printMap :: (Show k, Show v) => Map.Map k [v] -> IO ()
printMap kvs = do
  traverse_ (putStrLn . showMapEntry) $ Map.toList kvs
  where
    showMapEntry (k, vs) = show k ++ " -> " ++ intercalate ", " (show <$> vs)