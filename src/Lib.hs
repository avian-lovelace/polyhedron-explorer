module Lib
  ( main,
  )
where

import qualified Data.Either as Either
import Operators
import Platonic
import Semiregular
import Svg
import Unfold
import Volume

main :: IO ()
main = do
  let poly = volumeNormalizeOctahedral deltoidalIcositetrahedron
  let unfoldedVertex = unfoldVertexWhere poly (const True)
  let svgString = getSvgString unfoldedVertex
  let filePath = "./out/" ++ "output.svg"
  writeFile filePath svgString

-- case validatePolyhedron foo of
--   Left errorMessage -> putStrLn errorMessage
--   Right () -> do
--     displaySolid "output.svg" foo