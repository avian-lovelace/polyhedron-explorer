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
  let foo = dualPolyhedron . expandedPolyhedron $ icosahedron
  case validatePolyhedron foo of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      displaySolid "output.svg" foo