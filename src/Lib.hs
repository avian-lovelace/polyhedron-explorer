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
  let icosadodecahedron = amboPolyhedron dodecahedron
  case validatePolyhedron icosadodecahedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      displaySolid "output.svg" icosadodecahedron