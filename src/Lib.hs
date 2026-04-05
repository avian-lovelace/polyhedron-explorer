module Lib
  ( main,
  )
where

import qualified Data.Either as Either
import Operators
import Platonic
import Polyhedron
import Svg
import Unfold
import Volume

main :: IO ()
main = do
  let poly = volumeNormalizeOctahedral . dualPolyhedron . amboPolyhedron $ cube
  printPolyhedron poly
  print . polyhedronVolume $ poly
  let unfoldedVertex = unfoldVertexWhere poly Either.isLeft
  let svgString = getSvgString unfoldedVertex
  let filePath = "./out/" ++ "output.svg"
  writeFile filePath svgString

-- case validatePolyhedron foo of
--   Left errorMessage -> putStrLn errorMessage
--   Right () -> do
--     displaySolid "output.svg" foo