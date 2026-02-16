module Lib
  ( main,
  )
where

import Data.Foldable (traverse_)
import Data.List (intercalate)
import Operators
import Platonic
import Polyhedron
import Projection
import Svg
import Volume

main :: IO ()
main = do
  let faces = getFaces dodecahedron
  let faces2d = [faceTo2D . vectorizeFace $ f | f <- faces]
  -- let getFaceString face = intercalate ", " $ show . snd <$> face
  let svgString = getSvgString $ take 1 faces2d
  writeFile "./out/output.svg" svgString