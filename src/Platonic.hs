module Platonic
  ( tetrahedron,
    cube,
    octahedron,
    dodecahedron,
    icosahedron,
  )
where

import Polyhedron
import Space

tetrahedron :: Polyhedron (Sign, Sign, Sign) (Sign, Sign, Sign)
tetrahedron = generatePolyhedron vertexGenerators vgToPoint faceGenerators fgToFace
  where
    vertexGenerators =
      [ (xSign, ySign, zSign)
        | xSign <- plusMinus,
          ySign <- plusMinus,
          zSign <- plusMinus,
          signMultiplier xSign * signMultiplier ySign * signMultiplier zSign == 1
      ]
    vgToPoint (xSign, ySign, zSign) = Point (signMultiplier xSign, signMultiplier ySign, signMultiplier zSign)
    faceGenerators =
      [ (xSign, ySign, zSign)
        | xSign <- plusMinus,
          ySign <- plusMinus,
          zSign <- plusMinus,
          signMultiplier xSign * signMultiplier ySign * signMultiplier zSign == -1
      ]
    fgToFace (xSign, ySign, zSign) = filter (\(xSign', ySign', zSign') -> xSign == xSign' || ySign == ySign' || zSign == zSign') vertexGenerators

cube :: Polyhedron (Sign, Sign, Sign) (Axis, Sign)
cube = generatePolyhedron vertexGenerators vgToPoint faceGenerators fgToFace
  where
    vertexGenerators = [(xSign, ySign, zSign) | xSign <- plusMinus, ySign <- plusMinus, zSign <- plusMinus]
    vgToPoint (xSign, ySign, zSign) = Point (signMultiplier xSign, signMultiplier ySign, signMultiplier zSign)
    faceGenerators = [(axis, sign) | axis <- axes, sign <- plusMinus]
    faceGeneratorTemplate sign = [(sign, Pos, Pos), (sign, Pos, Neg), (sign, Neg, Neg), (sign, Neg, Pos)]
    fgToFace (axis, sign) = alignedToAxis axis <$> faceGeneratorTemplate sign

octahedron :: Polyhedron (Axis, Sign) (Sign, Sign, Sign)
octahedron = generatePolyhedron vertexGenerators vgToPoint faceGenerators fgToFace
  where
    vertexGenerators = [(axis, sign) | axis <- axes, sign <- plusMinus]
    vgToPoint (axis, sign) = timesSign sign (unitAxis axis)
    faceGenerators = [(xSign, ySign, zSign) | xSign <- plusMinus, ySign <- plusMinus, zSign <- plusMinus]
    fgToFace (xSign, ySign, zSign) = [(X, xSign), (Y, ySign), (Z, zSign)]

data DodecahedronVertex
  = OctantVertex (Sign, Sign, Sign)
  | AxisVertex Axis Sign Sign
  deriving (Eq, Ord)

dodecahedron :: Polyhedron DodecahedronVertex (Axis, Sign, Sign)
dodecahedron = generatePolyhedron vertexGenerators vgToPoint faceGenerators fgToFace
  where
    vertexGenerators =
      [OctantVertex (xSign, ySign, zSign) | xSign <- plusMinus, ySign <- plusMinus, zSign <- plusMinus]
        <> [AxisVertex axis majorAxisSign minorAxisSign | axis <- axes, majorAxisSign <- plusMinus, minorAxisSign <- plusMinus]
    phi = (sqrt 5 + 1) / 2
    phiInverse = (sqrt 5 - 1) / 2
    vgToPoint (OctantVertex (xSign, ySign, zSign)) = Point (signMultiplier xSign, signMultiplier ySign, signMultiplier zSign)
    vgToPoint (AxisVertex axis majorAxisSign minorAxisSign) =
      Point . alignedToAxis axis $ (signMultiplier majorAxisSign * phi, signMultiplier minorAxisSign * phiInverse, 0)
    faceGenerators = [(axis, majorAxisSign, minorAxisSign) | axis <- axes, majorAxisSign <- plusMinus, minorAxisSign <- plusMinus]
    fgToFace (axis, majorAxisSign, minorAxisSign) =
      [ AxisVertex axis majorAxisSign minorAxisSign,
        OctantVertex . alignedToAxis axis $ (majorAxisSign, minorAxisSign, minorAxisSign),
        AxisVertex (prevAxis axis) minorAxisSign majorAxisSign,
        OctantVertex . alignedToAxis axis $ (majorAxisSign, negativeSign minorAxisSign, minorAxisSign),
        AxisVertex axis majorAxisSign (negativeSign minorAxisSign)
      ]

icosahedron :: Polyhedron (Axis, Sign, Sign) DodecahedronVertex
icosahedron = generatePolyhedron vertexGenerators vgToPoint faceGenerators fgToFace
  where
    vertexGenerators = [(axis, majorAxisSign, minorAxisSign) | axis <- axes, majorAxisSign <- plusMinus, minorAxisSign <- plusMinus]
    phi = (sqrt 5 + 1) / 2
    vgToPoint (axis, majorAxisSign, minorAxisSign) =
      Point . alignedToAxis axis $ (signMultiplier majorAxisSign * phi, signMultiplier minorAxisSign, 0)
    faceGenerators =
      [OctantVertex (xSign, ySign, zSign) | xSign <- plusMinus, ySign <- plusMinus, zSign <- plusMinus]
        <> [AxisVertex axis majorAxisSign minorAxisSign | axis <- axes, majorAxisSign <- plusMinus, minorAxisSign <- plusMinus]
    fgToFace (OctantVertex (xSign, ySign, zSign)) =
      [ (X, xSign, ySign),
        (Y, ySign, zSign),
        (Z, zSign, xSign)
      ]
    fgToFace (AxisVertex axis majorAxisSign minorAxisSign) =
      [ (axis, majorAxisSign, majorAxisSign),
        (prevAxis axis, minorAxisSign, majorAxisSign),
        (axis, majorAxisSign, negativeSign majorAxisSign)
      ]