module Display
  ( displayWireframe,
    displaySolid,
  )
where

import qualified Data.Map as Map
import Polyhedron
import Projection
import Svg

displayWireframe :: (Ord v) => String -> Polyhedron v f -> IO ()
displayWireframe fileName polyhedron = do
  let faces = getPointFaces polyhedron
  let (n1, n2) = offsetNormalBasis
  let faces2d = [projectFace n1 n2 f | (_, f) <- Map.toList faces]
  let svgString = getSvgString faces2d
  let filePath = "./out/" ++ fileName
  writeFile filePath svgString

displaySolid :: (Ord v) => String -> Polyhedron v f -> IO ()
displaySolid fileName polyhedron = do
  let faces = getPointFaces polyhedron
  let (n1, n2) = offsetNormalBasis
  let faces2d = [projectFace n1 n2 f | (_, f) <- Map.toList faces, isPositivelyOriented n1 n2 f]
  let svgString = getSvgString faces2d
  let filePath = "./out/" ++ fileName
  writeFile filePath svgString