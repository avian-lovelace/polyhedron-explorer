module Operators
  ( dualPolyhedron,
    dualPolyhedron',
    amboPolyhedron,
    amboPolyhedron',
    expandedPolyhedron,
    ExpandedFace,
    isExpandedVertexFace,
    isExpandedFaceFace,
    truncatedPolyhedron,
  )
where

import Data.Map ((!))
import qualified Data.Map as Map
import GHC.Float (int2Double)
import Midsphere
import Pair
import Polyhedron
import Projection
import Space

{- Perform the dual operation, which produces a polyhedron with a vertex for each face of the original polyhedron and
  vice versa. This produces for example a octahedron from a cube and a dodecahdron from an icosahedron.

  This version of the dual operator produces the canonical dual, where the dual vertex positions are caulcuated as the
  projective reciprocation of the faces about the midsphere. This should produce a valid polyhedron for all polyhedra
  with a midsphere. This should include all Platonic, Archimedian, and Catalan polyhedra. -}
dualPolyhedron :: (Ord v, Ord f) => Polyhedron v f -> Polyhedron f v
dualPolyhedron polyhedron = Polyhedron {vertexPoints = vertexPoints', vertices = faces, faces = vertices}
  where
    Polyhedron {vertexPoints, vertices, faces} = polyhedron
    midRadius = getMidsphereRadius polyhedron
    dualVertex vertexOrder = getMidspherePolar midRadius p1 p2 p3
      where
        v1 : v2 : v3 : _ = vertexOrder
        p1 = vertexPoints ! v1
        p2 = vertexPoints ! v2
        p3 = vertexPoints ! v3
    vertexPoints' = Map.map dualVertex faces

{- Perform the dual operation, which produces a polyhedron with a vertex for each face of the original polyhedron and
  vice versa. This produces for example a octahedron from a cube and a dodecahdron from an icosahedron.

  This version of the dual operator produces uses a simplified vertex calculation algorithm, where the dual vertex
  positions are calculated as the centroids of the faces. This produces a valid polyhedron for all Platonic solids.
  However, for other solids, it may produce invalid polyhedra with non-flat faces. -}
dualPolyhedron' :: (Ord v) => Polyhedron v f -> Polyhedron f v
dualPolyhedron' (Polyhedron {vertexPoints, vertices, faces}) = Polyhedron {vertexPoints = faceCenters, vertices = faces, faces = vertices}
  where
    faceCenter vertexOrder = elementWise (/ (int2Double $ length vertexOrder)) $ foldr (elementWise' (+) . (vertexPoints !)) zero vertexOrder
    faceCenters = Map.map faceCenter faces

{- Perform the ambo operation, which produces a polyhedron with a vertex for each edge of the original polyhedron and a
  face for each vertex and face of the original polyhedron. This produces for example a cuboctahedron from a cube and a
  icosadodecahedron from an icosahedron.

  This version of the dual operator produces calculates the vertex positions as the centers of the edges. I don't have
  good intuition about when this does or doesn't produce valid polyhedra with flat faces. -}
amboPolyhedron :: (Ord v, Ord f) => Polyhedron v f -> Polyhedron (Edge v f) (Either v f)
amboPolyhedron polyhedron = Polyhedron {vertexPoints = vertexPoints', vertices = vertices', faces = faces'}
  where
    (Polyhedron {vertexPoints, vertices, faces}) = polyhedron
    edges = getEdges polyhedron
    facePairToEdgeMap = Map.fromList [(fPair, edge) | edge <- edges, let Edge _ fPair = edge]
    vertexPairToEdgeMap = Map.fromList [(vPair, edge) | edge <- edges, let Edge vPair _ = edge]
    vertexPoints' =
      Map.fromList
        [ (edge, edgeCenter)
          | edge <- edges,
            let Edge (Pair v1 v2) _ = edge,
            let p1 = vertexPoints ! v1,
            let p2 = vertexPoints ! v2,
            let edgeCenter = elementWise (/ 2) $ elementWise' (+) p1 p2
        ]
    vertices' =
      Map.fromList
        [ (edge, [Left v1, Right f1, Left v2, Right f2])
          | edge <- edges,
            let Edge (Pair v1 v2) (Pair f1 f2) = edge
        ]
    vertexFaces =
      [ (Left vertex, incidentEdges)
        | (vertex, orderedFaces) <- Map.toList vertices,
          let facePairs = getAdjacentPairs orderedFaces,
          let incidentEdges = [facePairToEdgeMap ! fPair | fPair <- facePairs]
      ]
    faceFaces =
      [ (Right face, incidentEdges)
        | (face, orderedVertices) <- Map.toList faces,
          let vertexPairs = getAdjacentPairs orderedVertices,
          let incidentEdges = [vertexPairToEdgeMap ! vPair | vPair <- vertexPairs]
      ]
    faces' = Map.fromList $ vertexFaces ++ faceFaces

{- Perform the ambo operation, which produces a polyhedron with a vertex for each edge of the original polyhedron and a
  face for each vertex and face of the original polyhedron. This produces for example a cuboctahedron from a cube and a
  icosadodecahedron from an icosahedron.

  This version of the dual operator produces calculates the vertex positions as the tangency points of the midsphere on
  each edge. The idea was that this might get you a rhombicosidodecahedron with square faces from a rhomic
  triacontahedron, but this did not work. I'm keeping this around because I feel like it might produce valid polyhedra
  with flat faces in some cases where the simpler method would fail, but I'm not sure if that's true. -}
amboPolyhedron' :: (Ord v, Ord f) => Polyhedron v f -> Polyhedron (Edge v f) (Either v f)
amboPolyhedron' polyhedron = Polyhedron {vertexPoints = vertexPoints', vertices = vertices', faces = faces'}
  where
    (Polyhedron {vertexPoints, vertices, faces}) = polyhedron
    edges = getEdges polyhedron
    facePairToEdgeMap = Map.fromList [(fPair, edge) | edge <- edges, let Edge _ fPair = edge]
    vertexPairToEdgeMap = Map.fromList [(vPair, edge) | edge <- edges, let Edge vPair _ = edge]
    vertexPoints' =
      Map.fromList
        [ (edge, midsphereTangent)
          | edge <- edges,
            let Edge (Pair v1 v2) _ = edge,
            let p1 = vertexPoints ! v1,
            let p2 = vertexPoints ! v2,
            let midsphereTangent = projectionOnLine p1 p2 zero
        ]
    vertices' =
      Map.fromList
        [ (edge, [Left v1, Right f1, Left v2, Right f2])
          | edge <- edges,
            let Edge (Pair v1 v2) (Pair f1 f2) = edge
        ]
    vertexFaces =
      [ (Left vertex, incidentEdges)
        | (vertex, orderedFaces) <- Map.toList vertices,
          let facePairs = getAdjacentPairs orderedFaces,
          let incidentEdges = [facePairToEdgeMap ! fPair | fPair <- facePairs]
      ]
    faceFaces =
      [ (Right face, incidentEdges)
        | (face, orderedVertices) <- Map.toList faces,
          let vertexPairs = getAdjacentPairs orderedVertices,
          let incidentEdges = [vertexPairToEdgeMap ! vPair | vPair <- vertexPairs]
      ]
    faces' = Map.fromList $ vertexFaces ++ faceFaces

{- Perform the expand operation, which produces a polyhedron with a face for every vertex, face, and edge of the
  original polyhedron. This is topologically equivalent to applying the ambo operation twice. This produces for example
  a rhombicuboctahedron from a cube and a rhombicosidodecahedron from an icosahedron.

  The vertex point calculation here produces a valid equilateral polyhedron for all Platonic solids. As the value of
  faceScaleFactor ranges from zero to one, you will get topologically equivalent polyhedra, but the edge faces will be
  rectanges of different dimensions.

  This method doesn't work for all Archimedean and Catalan solids, as the face scaling trick depends on the oritinal
  polyhedron and its dual both being equilateral. However, it should work for both the rhomic dodecahedron and the
  rhombic triacontahedron. -}
expandedPolyhedron :: (Ord v, Ord f) => Polyhedron v f -> Polyhedron (v, f) (ExpandedFace v f)
expandedPolyhedron polyhedron = Polyhedron {vertexPoints = vertexPoints', vertices = vertices', faces = faces'}
  where
    (Polyhedron {vertexPoints, vertices, faces}) = polyhedron
    edges = getEdges polyhedron
    faceCenters = Map.map (\vertexOrder -> elementWise (/ (int2Double $ length vertexOrder)) $ foldr (elementWise' (+) . (vertexPoints !)) zero vertexOrder) faces
    faceScaleFactor = faceCenterDistance / (edgeLength + faceCenterDistance)
      where
        Edge (Pair v1 v2) (Pair f1 f2) = head edges
        edgeLength = distance (vertexPoints ! v1) (vertexPoints ! v2)
        faceCenterDistance = distance (faceCenters ! f1) (faceCenters ! f2)
    vertexPoints' =
      Map.fromList
        [ ((vertex, face), vertexPoint')
          | (face, vertexOrder) <- Map.toList faces,
            vertex <- vertexOrder,
            let vertexPoint = vertexPoints ! vertex,
            let faceCenter = faceCenters ! face,
            let vertexVector = differenceVector faceCenter vertexPoint,
            let scaledVertexVector = faceScaleFactor /*/ vertexVector,
            let vertexPoint' = vectorEndpoint faceCenter scaledVertexVector
        ]
    vertices' =
      Map.fromList
        [ ((vertex, face), [VertexFace vertex, EdgeFace edge1, FaceFace face, EdgeFace edge2])
          | (face, vertexOrder) <- Map.toList faces,
            vertex <- vertexOrder,
            let [edge1, edge2] = filter (\(Edge pv pf) -> pairContains vertex pv && pairContains face pf) edges
        ]
    vertexFaces =
      [ (VertexFace vertex, map (vertex,) faceOrder)
        | (vertex, faceOrder) <- Map.toList vertices
      ]
    faceFaces =
      [ (FaceFace face, map (,face) vertexOrder)
        | (face, vertexOrder) <- Map.toList faces
      ]
    edgeFaces =
      [ (EdgeFace edge, [(v1, f1), (v1, f2), (v2, f2), (v2, f1)])
        | edge <- edges,
          let (Edge (Pair v1 v2) (Pair f1 f2)) = edge
      ]
    faces' = Map.fromList $ vertexFaces ++ faceFaces ++ edgeFaces

data ExpandedFace v f
  = VertexFace v
  | FaceFace f
  | EdgeFace (Edge v f)
  deriving (Eq, Ord)

isExpandedVertexFace :: ExpandedFace v f -> Bool
isExpandedVertexFace (VertexFace _) = True
isExpandedVertexFace _ = False

isExpandedFaceFace :: ExpandedFace v f -> Bool
isExpandedFaceFace (FaceFace _) = True
isExpandedFaceFace _ = False

truncatedPolyhedron :: (Ord v, Ord f) => Polyhedron v f -> Polyhedron (v, v) (Either v f)
truncatedPolyhedron polyhedron = Polyhedron {vertexPoints = vertexPoints', vertices = vertices', faces = faces'}
  where
    (Polyhedron {vertexPoints, faces}) = polyhedron
    edges = getEdges polyhedron
    scaleFactor = edgeLength / (2 * (edgeLength + midpointDistance))
      where
        (_, v1 : u : v2 : _) = head $ Map.toList faces
        p1 = vertexPoints ! v1
        q = vertexPoints ! u
        p2 = vertexPoints ! v2
        edgeLength = distance p1 q
        midpoint1 = elementWise (/ 2) $ elementWise' (+) p1 q
        midpoint2 = elementWise (/ 2) $ elementWise' (+) p2 q
        midpointDistance = distance midpoint1 midpoint2
    getvertexPoint' v1 v2 = vectorEndpoint p1 scaledVertexVector
      where
        p1 = vertexPoints ! v1
        p2 = vertexPoints ! v2
        edgeVector = differenceVector p1 p2
        scaledVertexVector = scaleFactor /*/ edgeVector
    vertexPoints' =
      Map.fromList . concat $
        [ [((v1, v2), getvertexPoint' v1 v2), ((v2, v1), getvertexPoint' v2 v1)]
          | Edge (Pair v1 v2) _ <- edges
        ]
    vertices' =
      Map.fromList . concat $
        [ [((v1, v2), [Left v1, Right f1, Right f2]), ((v2, v1), [Left v2, Right f1, Right f2])]
          | Edge (Pair v1 v2) (Pair f1 f2) <- edges
        ]
    faceFaces =
      [ (Right face, vertexOrder')
        | (face, vertexOrder) <- Map.toList faces,
          let adjacentVertexPairs = getAdjacentTuples vertexOrder,
          let vertexOrder' = [vertex' | (v1, v2) <- adjacentVertexPairs, vertex' <- [(v1, v2), (v2, v1)]]
      ]
    vertexFaces =
      [ (Left vertex, vertexOrder')
        | (vertex, adjacentVertexOrder) <- Map.toList $ getAdjacentVertexOrders polyhedron,
          let vertexOrder' = [(vertex, adjacentVertex) | adjacentVertex <- adjacentVertexOrder]
      ]
    faces' = Map.fromList $ faceFaces ++ vertexFaces