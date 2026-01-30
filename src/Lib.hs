module Lib
  ( main,
  )
where

import Platonic
import Polyhedron
import Volume

main :: IO ()
main = do
  printPolyhedron icosahedron
  print . polyhedronVolume $ icosahedron
