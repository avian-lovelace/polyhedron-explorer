module Space
  ( Point (..),
    zero,
    elementWise,
    elementWise',
    Axis (..),
    axes,
    unitAxis,
    alignedToAxis,
    prevAxis,
    nextAxis,
    Sign (..),
    plusMinus,
    getSign,
    signMultiplier,
    timesSign,
    negativeSign,
    Point2D (..),
    Polygon (..),
    translateToZero,
    rotodilateToMatch,
    matchSegments,
    reflectOverSegment,
    applyTransform,
    polygonCenter,
    distance2D,
  )
where

import Data.Complex (Complex (..))
import qualified Data.Complex as Complex

newtype Point = Point (Double, Double, Double)

-- show (Point (x, y, z)) = show (getSign x, getSign y, getSign z)

instance Show Point where
  show (Point p) = show p

zero :: Point
zero = Point (0, 0, 0)

elementWise :: (Double -> Double) -> Point -> Point
elementWise f (Point (x, y, z)) = Point (f x, f y, f z)

elementWise' :: (Double -> Double -> Double) -> Point -> Point -> Point
elementWise' f (Point (x1, y1, z1)) (Point (x2, y2, z2)) = Point (f x1 x2, f y1 y2, f z1 z2)

data Axis = X | Y | Z deriving (Eq, Ord, Show)

axes :: [Axis]
axes = [X, Y, Z]

unitAxis :: Axis -> Point
unitAxis X = Point (1, 0, 0)
unitAxis Y = Point (0, 1, 0)
unitAxis Z = Point (0, 0, 1)

alignedToAxis :: Axis -> (a, a, a) -> (a, a, a)
alignedToAxis X (r, s, t) = (r, s, t)
alignedToAxis Y (r, s, t) = (t, r, s)
alignedToAxis Z (r, s, t) = (s, t, r)

prevAxis :: Axis -> Axis
prevAxis X = Z
prevAxis Y = X
prevAxis Z = Y

nextAxis :: Axis -> Axis
nextAxis X = Y
nextAxis Y = Z
nextAxis Z = X

data Sign = Neg | Zero | Pos deriving (Eq, Ord)

instance Show Sign where
  show Pos = "+"
  show Zero = "0"
  show Neg = "-"

plusMinus :: [Sign]
plusMinus = [Pos, Neg]

getSign :: Double -> Sign
getSign x
  | x > 0 = Pos
  | x == 0 = Zero
  | otherwise = Neg

signMultiplier :: (Num a) => Sign -> a
signMultiplier Pos = 1
signMultiplier Zero = 0
signMultiplier Neg = -1

timesSign :: Sign -> Point -> Point
timesSign Pos (Point (x, y, z)) = Point (x, y, z)
timesSign Zero _ = Point (0, 0, 0)
timesSign Neg (Point (x, y, z)) = Point (-x, -y, -z)

negativeSign :: Sign -> Sign
negativeSign Pos = Neg
negativeSign Zero = Zero
negativeSign Neg = Pos

newtype Point2D = Point2D (Double, Double)

instance Show Point2D where
  show (Point2D (x, y)) = "(" ++ show x ++ ", " ++ show y ++ ")"

newtype Polygon v = Polygon [(v, Point2D)]

type Transform2D = Point2D -> Point2D

translateToZero :: Point2D -> Transform2D
translateToZero (Point2D (baseX, baseY)) (Point2D (x, y)) = Point2D (x - baseX, y - baseY)

rotodilateToMatch :: Point2D -> Point2D -> Transform2D
rotodilateToMatch (Point2D (baseX, baseY)) (Point2D (targetX, targetY)) (Point2D (x, y)) =
  Point2D (Complex.realPart tranformedZ, Complex.imagPart tranformedZ)
  where
    baseZ = baseX :+ baseY
    targetZ = targetX :+ targetY
    z = x :+ y
    tranformedZ = (targetZ / baseZ) * z

-- Implicitly, target 1 is the origin
matchSegments :: Point2D -> Point2D -> Point2D -> Transform2D
matchSegments base1 base2 target2 = rotodilation . translation
  where
    translation = translateToZero base1
    base2' = translation base2
    rotodilation = rotodilateToMatch base2' target2

-- Implicitly, axis1 is the origin
reflectOverSegment :: Point2D -> Transform2D
reflectOverSegment axis2 = inverseRotation . reflectOverXAxis . rotation
  where
    rotation = matchSegments (Point2D (0, 0)) axis2 (Point2D (1, 0))
    reflectOverXAxis (Point2D (x, y)) = Point2D (x, -y)
    inverseRotation = matchSegments (Point2D (0, 0)) (Point2D (1, 0)) axis2

applyTransform :: Transform2D -> Polygon v -> Polygon v
applyTransform transform (Polygon points) = Polygon [(v, transform p) | (v, p) <- points]

polygonCenter :: Polygon v -> Point2D
polygonCenter (Polygon points) = Point2D (sumX / numPoints, sumY / numPoints)
  where
    coordinatePairs = [point | (_, Point2D point) <- points]
    sumX = sum $ fst <$> coordinatePairs
    sumY = sum $ snd <$> coordinatePairs
    numPoints = fromIntegral $ length coordinatePairs

distance2D :: Point2D -> Point2D -> Double
distance2D (Point2D (x1, y1)) (Point2D (x2, y2)) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)