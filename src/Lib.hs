module Lib
  ( main,
  )
where

import Display
import Operators
import Platonic
import Validation

main :: IO ()
main = do
  let rhombic = dualPolyhedron . amboPolyhedron $ dodecahedron
  let foo = expandedPolyhedron rhombic
  case validatePolyhedron foo of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      displaySolid "output.svg" foo