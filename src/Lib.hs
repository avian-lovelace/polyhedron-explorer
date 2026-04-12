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
  runCuboctahedron
  runRhombicDodecahedron
  runIcosadodecahedron
  runRhombicTriacontahedron
  runRhomicuboctahedron
  runDeltoidalIcositetrahedron
  runRhombicosidodecahedron
  runDeltoidalHexecontahedron

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

runCuboctahedron :: IO ()
runCuboctahedron = do
  let polyhedron = volumeNormalizeOctahedral cuboctahedron
  let name = "cuboctahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runRhombicDodecahedron :: IO ()
runRhombicDodecahedron = do
  let polyhedron = volumeNormalizeOctahedral rhombicDodecahedron
  let name = "rhombic-dodecahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name
      let unfoldedVertex = unfoldVertexWhere polyhedron isLeft
      let patternSvgString = getSvgString unfoldedVertex
      let patternFilePath = "./patterns/" ++ name ++ "-pattern.svg"
      writeFile patternFilePath patternSvgString

runIcosadodecahedron :: IO ()
runIcosadodecahedron = do
  let polyhedron = volumeNormalizeIcosahedral icosadodecahedron
  let name = "icosadodecahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runRhombicTriacontahedron :: IO ()
runRhombicTriacontahedron = do
  let polyhedron = volumeNormalizeIcosahedral rhombicTriacontahedron
  let name = "rhombic-triacontahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name
      let unfoldedVertex = unfoldVertexWhere polyhedron isLeft
      let patternSvgString = getSvgString unfoldedVertex
      let patternFilePath = "./patterns/" ++ name ++ "-pattern.svg"
      writeFile patternFilePath patternSvgString

runRhomicuboctahedron :: IO ()
runRhomicuboctahedron = do
  let polyhedron = volumeNormalizeOctahedral rhombicuboctahedron
  let name = "rhombicuboctahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runDeltoidalIcositetrahedron :: IO ()
runDeltoidalIcositetrahedron = do
  let polyhedron = volumeNormalizeOctahedral deltoidalIcositetrahedron
  let name = "deltoidal-icositetrahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name
      let unfoldedVertex = unfoldVertexWhere polyhedron isExpandedVertexFace
      let patternSvgString = getSvgString unfoldedVertex
      let patternFilePath = "./patterns/" ++ name ++ "-pattern.svg"
      writeFile patternFilePath patternSvgString

runRhombicosidodecahedron :: IO ()
runRhombicosidodecahedron = do
  let polyhedron = volumeNormalizeIcosahedral rhombicosidodecahedron
  let name = "rhombicosidodecahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runDeltoidalHexecontahedron :: IO ()
runDeltoidalHexecontahedron = do
  let polyhedron = volumeNormalizeIcosahedral deltoidalHexecontahedron
  let name = "deltoidal-hexecontahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name
      let unfoldedVertex = unfoldVertexWhere polyhedron isExpandedFaceFace
      let patternSvgString = getSvgString unfoldedVertex
      let patternFilePath = "./patterns/" ++ name ++ "-pattern.svg"
      writeFile patternFilePath patternSvgString