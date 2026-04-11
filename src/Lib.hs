module Lib
  ( main,
  )
where

import Data.Either (isRight)
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
  let poly = volumeNormalizeIcosahedral . dualPolyhedron . truncatedPolyhedron $ cube
  let unfoldedVertex = unfoldVertexWhere poly isRight
  let svgString = getSvgString unfoldedVertex
  let filePath = "./out/" ++ "output.svg"
  writeFile filePath svgString

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