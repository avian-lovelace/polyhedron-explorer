module Display
  ( outputRenders,
  )
where

import qualified Data.Map as Map
import Polyhedron
import Projection
import Svg

renderWireframeSvg :: (Ord v) => Polyhedron v f -> String
renderWireframeSvg polyhedron = svgString
  where
    faces = getPointFaces polyhedron
    (n1, n2) = offsetNormalBasis
    faces2d = [projectFace n1 n2 f | (_, f) <- Map.toList faces]
    svgString = getSvgString faces2d

renderSolidSvg :: (Ord v) => Polyhedron v f -> String
renderSolidSvg polyhedron = svgString
  where
    faces = getPointFaces polyhedron
    (n1, n2) = offsetNormalBasis
    faces2d = [projectFace n1 n2 f | (_, f) <- Map.toList faces, isPositivelyOriented n1 n2 f]
    svgString = getSvgString faces2d

outputRenders :: (Ord v) => Polyhedron v f -> [Char] -> IO ()
outputRenders polyhedron name = do
  let solidFilePath = "./renders/" ++ name ++ "-solid.svg"
  let wireframeFilePath = "./renders/" ++ name ++ "-wireframe.svg"
  let solidRenderSvgString = renderSolidSvg polyhedron
  let wireframeRenderSvgString = renderWireframeSvg polyhedron
  writeFile solidFilePath solidRenderSvgString
  writeFile wireframeFilePath wireframeRenderSvgString