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
  let icosadodecahedron = amboPolyhedron cube
  let triacontahedron = dualPolyhedron icosadodecahedron
  case validatePolyhedron triacontahedron of
    Left errorMessage -> putStrLn errorMessage
    Right () -> do
      displaySolid "output.svg" triacontahedron