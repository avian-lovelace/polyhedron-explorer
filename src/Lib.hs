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
  let faces = getFaces icosahedron
  let (n1, n2) = offsetNormalBasis
  let faces2d = [projectFace n1 n2 f | f <- faces]
  -- let getFaceString face = intercalate ", " $ show . snd <$> face
  let svgString = getSvgString faces2d
  writeFile "./out/output.svg" svgString