module Display
  ( displayWireframe,
    displaySolid,
  )
where

import Polyhedron
import Projection
import Svg

displayWireframe :: (Ord v) => String -> Polyhedron v f -> IO ()
displayWireframe fileName polyhedron = do
  let faces = getFaces polyhedron
  let (n1, n2) = offsetNormalBasis
  let faces2d = [projectFace n1 n2 f | f <- faces]
  let svgString = getSvgString faces2d
  let filePath = "./out/" ++ fileName
  writeFile filePath svgString

displaySolid :: (Ord v) => String -> Polyhedron v f -> IO ()
displaySolid fileName polyhedron = do
  let faces = getFaces polyhedron
  let (n1, n2) = offsetNormalBasis
  let faces2d = [projectFace n1 n2 f | f <- faces, isPositivelyOriented n1 n2 f]
  let svgString = getSvgString faces2d
  let filePath = "./out/" ++ fileName
  writeFile filePath svgString