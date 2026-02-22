{-# OPTIONS_GHC -Wno-type-defaults #-}

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

type Octant = (Sign, Sign, Sign)

type SignedAxis = (Axis, Sign)

tetrahedron :: Polyhedron (Sign, Sign, Sign) (Sign, Sign, Sign)
tetrahedron = generatePolyhedron vertexGenerators vgToPoint vgToFaceOrder faceGenerators fgToVertexOrder
  where
    vertexGenerators =
      [ (xSign, ySign, zSign)
        | xSign <- plusMinus,
          ySign <- plusMinus,
          zSign <- plusMinus,
          signMultiplier xSign * signMultiplier ySign * signMultiplier zSign == 1
      ]
    vgToPoint (xSign, ySign, zSign) = Point (signMultiplier xSign, signMultiplier ySign, signMultiplier zSign)
    vgToFaceOrder (xSign, ySign, zSign) = filter (\(xSign', ySign', zSign') -> xSign == xSign' || ySign == ySign' || zSign == zSign') faceGenerators
    faceGenerators =
      [ (xSign, ySign, zSign)
        | xSign <- plusMinus,
          ySign <- plusMinus,
          zSign <- plusMinus,
          signMultiplier xSign * signMultiplier ySign * signMultiplier zSign == -1
      ]
    fgToVertexOrder (xSign, ySign, zSign) = filter (\(xSign', ySign', zSign') -> xSign == xSign' || ySign == ySign' || zSign == zSign') vertexGenerators

cubeFace :: SignedAxis -> [Octant]
cubeFace (axis, sign) = alignedToAxis axis <$> [(sign, Pos, Pos), (sign, Pos, Neg), (sign, Neg, Neg), (sign, Neg, Pos)]

octahedronFace :: Octant -> [SignedAxis]
octahedronFace (xSign, ySign, zSign) = [(X, xSign), (Y, ySign), (Z, zSign)]

cube :: Polyhedron Octant SignedAxis
cube = generatePolyhedron vertexGenerators vgToPoint vgToFaceOrder faceGenerators fgToVertexOrder
  where
    vertexGenerators = [(xSign, ySign, zSign) | xSign <- plusMinus, ySign <- plusMinus, zSign <- plusMinus]
    vgToPoint (xSign, ySign, zSign) = Point (signMultiplier xSign, signMultiplier ySign, signMultiplier zSign)
    vgToFaceOrder = octahedronFace
    faceGenerators = [(axis, sign) | axis <- axes, sign <- plusMinus]
    fgToVertexOrder = cubeFace

octahedron :: Polyhedron SignedAxis Octant
octahedron = generatePolyhedron vertexGenerators vgToPoint vgToFaceOrder faceGenerators fgToVertexOrder
  where
    vertexGenerators = [(axis, sign) | axis <- axes, sign <- plusMinus]
    vgToPoint (axis, sign) = timesSign sign (unitAxis axis)
    vgToFaceOrder = cubeFace
    faceGenerators = [(xSign, ySign, zSign) | xSign <- plusMinus, ySign <- plusMinus, zSign <- plusMinus]
    fgToVertexOrder = octahedronFace

data DodecahedronVertex
  = OctantVertex Octant
  | AxisVertex Axis Sign Sign
  deriving (Eq, Ord, Show)

type IcosahedronVertex = (Axis, Sign, Sign)

dodecahedronFace :: IcosahedronVertex -> [DodecahedronVertex]
dodecahedronFace (axis, majorAxisSign, minorAxisSign) =
  [ AxisVertex axis majorAxisSign minorAxisSign,
    OctantVertex . alignedToAxis axis $ (majorAxisSign, minorAxisSign, minorAxisSign),
    AxisVertex (prevAxis axis) minorAxisSign majorAxisSign,
    OctantVertex . alignedToAxis axis $ (majorAxisSign, negativeSign minorAxisSign, minorAxisSign),
    AxisVertex axis majorAxisSign (negativeSign minorAxisSign)
  ]

icosahedronFace :: DodecahedronVertex -> [IcosahedronVertex]
icosahedronFace (OctantVertex (xSign, ySign, zSign)) =
  [ (X, xSign, zSign),
    (Y, ySign, xSign),
    (Z, zSign, ySign)
  ]
icosahedronFace (AxisVertex axis majorAxisSign minorAxisSign) =
  [ (axis, majorAxisSign, majorAxisSign),
    (nextAxis axis, minorAxisSign, majorAxisSign),
    (axis, majorAxisSign, negativeSign majorAxisSign)
  ]

dodecahedron :: Polyhedron DodecahedronVertex IcosahedronVertex
dodecahedron = generatePolyhedron vertexGenerators vgToPoint vgToFaceOrder faceGenerators fgToVertexOrder
  where
    vertexGenerators =
      [OctantVertex (xSign, ySign, zSign) | xSign <- plusMinus, ySign <- plusMinus, zSign <- plusMinus]
        <> [AxisVertex axis majorAxisSign minorAxisSign | axis <- axes, majorAxisSign <- plusMinus, minorAxisSign <- plusMinus]
    phi = (sqrt 5 + 1) / 2
    phiInverse = (sqrt 5 - 1) / 2
    vgToPoint (OctantVertex (xSign, ySign, zSign)) = Point (signMultiplier xSign, signMultiplier ySign, signMultiplier zSign)
    vgToPoint (AxisVertex axis majorAxisSign minorAxisSign) =
      Point . alignedToAxis axis $ (signMultiplier majorAxisSign * phi, signMultiplier minorAxisSign * phiInverse, 0)
    vgToFaceOrder = icosahedronFace
    faceGenerators = [(axis, majorAxisSign, minorAxisSign) | axis <- axes, majorAxisSign <- plusMinus, minorAxisSign <- plusMinus]
    fgToVertexOrder = dodecahedronFace

icosahedron :: Polyhedron IcosahedronVertex DodecahedronVertex
icosahedron = generatePolyhedron vertexGenerators vgToPoint vgToFaceOrder faceGenerators fgToVertexOrder
  where
    vertexGenerators = [(axis, majorAxisSign, minorAxisSign) | axis <- axes, majorAxisSign <- plusMinus, minorAxisSign <- plusMinus]
    phi = (sqrt 5 + 1) / 2
    vgToPoint (axis, majorAxisSign, minorAxisSign) =
      Point . alignedToAxis axis $ (signMultiplier majorAxisSign * phi, signMultiplier minorAxisSign, 0)
    vgToFaceOrder = dodecahedronFace
    faceGenerators =
      [OctantVertex (xSign, ySign, zSign) | xSign <- plusMinus, ySign <- plusMinus, zSign <- plusMinus]
        <> [AxisVertex axis majorAxisSign minorAxisSign | axis <- axes, majorAxisSign <- plusMinus, minorAxisSign <- plusMinus]
    fgToVertexOrder = icosahedronFace