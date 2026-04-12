module Lib
  ( main,
  )
where

import Data.Either (isLeft, isRight)
import Display
import Operators
import Platonic
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
  runTruncatedTetrahedron
  runTriakisTetrahedron
  runTruncatedCube
  runTriakisOctahedron
  runTruncatedOctahedron
  runTetrakisHexahedron
  runTruncatedDodecahedron
  runTriakisIcosahedron
  runTruncatedIcosahedron
  runPentakisDodecahedron

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
      let unfoldedVertex1 = unfoldVertexWhere polyhedron isLeft
      let patternSvgString1 = getSvgString unfoldedVertex1
      let patternFilePath1 = "./patterns/" ++ name ++ "-pattern-1.svg"
      writeFile patternFilePath1 patternSvgString1
      let unfoldedVertex2 = take 3 $ unfoldVertexWhere polyhedron isRight
      let patternSvgString2 = getSvgString unfoldedVertex2
      let patternFilePath2 = "./patterns/" ++ name ++ "-pattern-2.svg"
      writeFile patternFilePath2 patternSvgString2

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

runTruncatedTetrahedron :: IO ()
runTruncatedTetrahedron = do
  let polyhedron = volumeNormalizeOctahedral truncatedTetrahedron
  let name = "truncated-tetrahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runTriakisTetrahedron :: IO ()
runTriakisTetrahedron = do
  let polyhedron = volumeNormalizeOctahedral triakisTetrahedron
  let name = "triakis-tetrahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runTruncatedCube :: IO ()
runTruncatedCube = do
  let polyhedron = volumeNormalizeOctahedral truncatedCube
  let name = "truncated-cube"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runTriakisOctahedron :: IO ()
runTriakisOctahedron = do
  let polyhedron = volumeNormalizeOctahedral triakisOctahedron
  let name = "triakis-octahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name
      let unfoldedVertex = take 6 $ unfoldVertexWhere polyhedron isRight
      let patternSvgString = getSvgString unfoldedVertex
      let patternFilePath = "./patterns/" ++ name ++ "-pattern.svg"
      writeFile patternFilePath patternSvgString

runTruncatedOctahedron :: IO ()
runTruncatedOctahedron = do
  let polyhedron = volumeNormalizeOctahedral truncatedOctahedron
  let name = "truncated-octahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runTetrakisHexahedron :: IO ()
runTetrakisHexahedron = do
  let polyhedron = volumeNormalizeOctahedral tetrakisHexahedron
  let name = "tetrakis-hexahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name
      let unfoldedVertex = unfoldVertexWhere polyhedron isRight
      let patternSvgString = getSvgString unfoldedVertex
      let patternFilePath = "./patterns/" ++ name ++ "-pattern.svg"
      writeFile patternFilePath patternSvgString

runTruncatedDodecahedron :: IO ()
runTruncatedDodecahedron = do
  let polyhedron = volumeNormalizeIcosahedral truncatedDodecahedron
  let name = "truncated-dodecahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runTriakisIcosahedron :: IO ()
runTriakisIcosahedron = do
  let polyhedron = volumeNormalizeIcosahedral triakisIcosahedron
  let name = "triakis-icosahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runTruncatedIcosahedron :: IO ()
runTruncatedIcosahedron = do
  let polyhedron = volumeNormalizeIcosahedral truncatedIcosahedron
  let name = "truncated-icosahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name

runPentakisDodecahedron :: IO ()
runPentakisDodecahedron = do
  let polyhedron = volumeNormalizeIcosahedral pentakisDodecahedron
  let name = "pentakis-dodecahedron"
  case validatePolyhedron polyhedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      outputRenders polyhedron name