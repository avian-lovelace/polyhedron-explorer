module Lib
  ( main,
  )
where

import Operators
import Platonic
import Svg
import Unfold

main :: IO ()
main = do
  let poly = dualPolyhedron . amboPolyhedron $ icosahedron
  let unfoldedVertex = unfoldVertexWhere poly (const True)
  let svgString = getSvgString unfoldedVertex
  let filePath = "./out/" ++ "output.svg"
  writeFile filePath svgString

-- case validatePolyhedron foo of
--   Left errorMessage -> putStrLn errorMessage
--   Right () -> do
--     displaySolid "output.svg" foo