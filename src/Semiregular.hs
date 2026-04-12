module Semiregular
  ( cuboctahedron,
    rhombicDodecahedron,
    icosadodecahedron,
    rhombicTriacontahedron,
    rhombicuboctahedron,
    deltoidalIcositetrahedron,
    rhombicosidodecahedron,
    deltoidalHexecontahedron,
    truncatedTetrahedron,
    triakisTetrahedron,
    truncatedCube,
    triakisOctahedron,
    truncatedOctahedron,
    tetrakisHexahedron,
    truncatedDodecahedron,
    triakisIcosahedron,
    truncatedIcosahedron,
    pentakisDodecahedron,
  )
where

import Operators
import Platonic
import Polyhedron
import Space

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

truncatedTetrahedron :: Polyhedron ((Sign, Sign, Sign), (Sign, Sign, Sign)) (Either (Sign, Sign, Sign) (Sign, Sign, Sign))
truncatedTetrahedron = truncatedPolyhedron tetrahedron

triakisTetrahedron :: Polyhedron (Either (Sign, Sign, Sign) (Sign, Sign, Sign)) ((Sign, Sign, Sign), (Sign, Sign, Sign))
triakisTetrahedron = dualPolyhedron truncatedTetrahedron

truncatedCube :: Polyhedron (Octant, Octant) (Either Octant SignedAxis)
truncatedCube = truncatedPolyhedron cube

triakisOctahedron :: Polyhedron (Either Octant SignedAxis) (Octant, Octant)
triakisOctahedron = dualPolyhedron truncatedCube

truncatedOctahedron :: Polyhedron (SignedAxis, SignedAxis) (Either SignedAxis Octant)
truncatedOctahedron = truncatedPolyhedron octahedron

tetrakisHexahedron :: Polyhedron (Either SignedAxis Octant) (SignedAxis, SignedAxis)
tetrakisHexahedron = dualPolyhedron truncatedOctahedron

truncatedDodecahedron :: Polyhedron (DodecahedronVertex, DodecahedronVertex) (Either DodecahedronVertex IcosahedronVertex)
truncatedDodecahedron = truncatedPolyhedron dodecahedron

triakisIcosahedron :: Polyhedron (Either DodecahedronVertex IcosahedronVertex) (DodecahedronVertex, DodecahedronVertex)
triakisIcosahedron = dualPolyhedron truncatedDodecahedron

truncatedIcosahedron :: Polyhedron (IcosahedronVertex, IcosahedronVertex) (Either IcosahedronVertex DodecahedronVertex)
truncatedIcosahedron = truncatedPolyhedron icosahedron

pentakisDodecahedron :: Polyhedron (Either IcosahedronVertex DodecahedronVertex) (IcosahedronVertex, IcosahedronVertex)
pentakisDodecahedron = dualPolyhedron truncatedIcosahedron