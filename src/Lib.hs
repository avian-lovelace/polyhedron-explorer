module Lib
  ( main,
  )
where

import Data.Either (isLeft, isRight)
import qualified Data.Either as Either
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Display
import Operators
import Platonic
import Polyhedron
import Semiregular
import Svg
import Unfold
import Validation
import Volume

main :: IO ()
main = do
  runTetrahedron
  runCube
  runOctahedron
  runDodecahedron
  runIcosahedron

runTetrahedron :: IO ()
runTetrahedron = do
  let polyhedron = volumeNormalizeOctahedral tetrahedron
  let name = "tetrahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runCube :: IO ()
runCube = do
  let polyhedron = volumeNormalizeOctahedral cube
  let name = "cube"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runOctahedron :: IO ()
runOctahedron = do
  let polyhedron = volumeNormalizeOctahedral octahedron
  let name = "octrahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runDodecahedron :: IO ()
runDodecahedron = do
  let polyhedron = volumeNormalizeIcosahedral dodecahedron
  let name = "dodecahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runIcosahedron :: IO ()
runIcosahedron = do
  let polyhedron = volumeNormalizeIcosahedral icosahedron
  let name = "icosahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

-- let Polyhedron {vertices, faces} = poly
-- putStrLn "Vertices:"
-- traverse_ print $ Map.toList vertices
-- putStrLn "Faces:"
-- traverse_ print $ Map.toList faces
-- case validatePolyhedron poly of
--   Left errorMessage -> putStrLn errorMessage
--   Right () -> do
--     displaySolid "output.svg" poly

-- let unfoldedVertex = unfoldVertexWhere poly (const True)
-- let svgString = getSvgString unfoldedVertex
-- let filePath = "./out/" ++ "output.svg"
-- writeFile filePath svgString

-- case validatePolyhedron foo of
--   Left errorMessage -> putStrLn errorMessage
--   Right () -> do
--     displaySolid "output.svg" foo