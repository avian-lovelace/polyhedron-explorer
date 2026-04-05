module Semiregular
  ( cuboctahedron,
    rhombicDodecahedron,
    icosadodecahedron,
    rhombicTriacontahedron,
    rhombicuboctahedron,
    deltoidalIcositetrahedron,
    rhombicosidodecahedron,
    deltoidalHexecontahedron,
  )
where

import Operators
import Platonic
import Polyhedron

cuboctahedron :: Polyhedron (Edge Octant SignedAxis) (Either Octant SignedAxis)
cuboctahedron = amboPolyhedron cube

rhombicDodecahedron :: Polyhedron (Either Octant SignedAxis) (Edge Octant SignedAxis)
rhombicDodecahedron = dualPolyhedron cuboctahedron

icosadodecahedron :: Polyhedron (Edge DodecahedronVertex IcosahedronVertex) (Either DodecahedronVertex IcosahedronVertex)
icosadodecahedron = amboPolyhedron dodecahedron

rhombicTriacontahedron :: Polyhedron (Either DodecahedronVertex IcosahedronVertex) (Edge DodecahedronVertex IcosahedronVertex)
rhombicTriacontahedron = dualPolyhedron icosadodecahedron

rhombicuboctahedron :: Polyhedron (Octant, SignedAxis) (ExpandedFace Octant SignedAxis)
rhombicuboctahedron = expandedPolyhedron cube

deltoidalIcositetrahedron :: Polyhedron (ExpandedFace Octant SignedAxis) (Octant, SignedAxis)
deltoidalIcositetrahedron = dualPolyhedron rhombicuboctahedron

rhombicosidodecahedron :: Polyhedron (DodecahedronVertex, IcosahedronVertex) (ExpandedFace DodecahedronVertex IcosahedronVertex)
rhombicosidodecahedron = expandedPolyhedron dodecahedron

deltoidalHexecontahedron :: Polyhedron (ExpandedFace DodecahedronVertex IcosahedronVertex) (DodecahedronVertex, IcosahedronVertex)
deltoidalHexecontahedron = dualPolyhedron rhombicosidodecahedron