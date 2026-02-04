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
import Volume

main :: IO ()
main = do
  let faces = getFaces dodecahedron
  let faces2d = [faceTo2D . vectorizeFace $ f | f <- faces]
  let getFaceString face = intercalate ", " $ show . snd <$> face
  let printFace face = putStrLn $ getFaceString face
  traverse_ printFace faces2d