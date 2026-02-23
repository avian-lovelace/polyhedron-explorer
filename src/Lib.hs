module Lib
  ( main,
  )
where

import Display (displaySolid)
import Operators
import Platonic

main :: IO ()
main = do
  let icosadodecahedron = amboPolyhedron dodecahedron
  displaySolid "output.svg" icosadodecahedron