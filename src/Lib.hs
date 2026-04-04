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

main :: IO ()
main = do
  let poly = rescale 150 . dualPolyhedron . amboPolyhedron $ icosahedron
  let unfoldedVertex = unfoldVertexWhere poly Either.isLeft
  let svgString = getSvgString $ take 3 unfoldedVertex
  let filePath = "./out/" ++ "output.svg"
  writeFile filePath svgString

-- case validatePolyhedron foo of
--   Left errorMessage -> putStrLn errorMessage
--   Right () -> do
--     displaySolid "output.svg" foo