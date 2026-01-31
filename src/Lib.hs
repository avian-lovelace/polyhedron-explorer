module Lib
  ( main,
  )
where

import Operators
import Platonic
import Polyhedron
import Volume

main :: IO ()
main = do
  let dual = dualPolyhedron octahedron
  printPolyhedron dual
  print . polyhedronVolume $ dual
