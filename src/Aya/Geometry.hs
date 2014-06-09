{-# LANGUAGE NoImplicitPrelude #-}

--
-- Geometry:
--

module Aya.Geometry
  ( Inout(..)
  , Ray
  , rpos
  , rdir
  , initRay
  , target
  , Shape
  , initPlain
  , initSphere
  , initPolygon
  , poly_v
  , poly_e1
  , poly_e2
  , poly_n
  , CsgOpe(..)
  , PreDistance
  , frontObjects
  , distance
  , side
  , getNormal
  )  where

import Data.Maybe
import NumericPrelude

import Aya.Algebra

-- inside/outside
-----------------------------------------------------------

data Inout = Inside | Outside deriving Eq

instance Show Inout where
  show a
    | a == Inside  = "Inside"
    | a == Outside = "Outside"

isInside :: Inout -> Bool
isInside io
  | io == Inside = True
  | otherwise    = False

-- ray 
----------------------------------------------------------

data Ray = Ray {rpos :: Point3, rdir :: Direction3} deriving Show

instance Eq Ray where
  (==) a b = (rpos a == rpos b) && (rdir a == rdir b)

initRay :: Point3 -> Direction3 -> Maybe Ray
initRay pos dir
  | ndir == Nothing  = Nothing
  | otherwise        = Just $ Ray pos $ fromJust ndir
  where
    ndir = normal dir

target :: Ray -> Double -> Vector3
target (Ray pos dir) t = pos + (dir <*> t)

-- shape class
-----------------------------------------------------------


data Shape =
    Plain Direction3 Double
  | Sphere Point3 Double
  | Polygon {
      poly_v  :: Point3
    , poly_e1 :: Point3
    , poly_e2 :: Point3
    , poly_n  :: Point3
    }
  | Csg CsgOpe Shape Shape

data CsgOpe = CsgOr | CsgAnd | CsgSub | CsgXor deriving Eq

type PreDistance = (Double, Shape, Inout)

instance Eq Shape where
  (==) (Plain n1 d1) (Plain n2 d2) = (n1 == n2) && (d1 == d2)
  (==) (Sphere c1 r1) (Sphere c2 r2) = (c1 == c2) && (r1 == r2)
  (==) (Polygon v e1 e2 n) (Polygon v' e1' e2' n') =
      (v == v') && (e1 == e1') && (e2 == e2') && (n == n')
  (==) (Csg o1 sa1 sb1) (Csg o2 sa2 sb2) =
      (o1 == o2) && (sa1 == sa2) && (sb1 == sb2)
  (==) a b = False

instance Show Shape where
  show (Plain n d) = "[" ++ (show n) ++ "," ++ (show d) ++ "]"
  show (Sphere c r) = "[" ++ show c ++ "," ++ show r ++ "]"
  show (Polygon v e1 e2 n) =
      "[" ++ show v ++ "," ++ show e1 ++ "," ++ show e2 ++ "," ++ show n ++ "]"

initPlain :: Vector3 -> Double -> Maybe Shape
initPlain n d
  | n == zero = Nothing
  | otherwise = Just (Plain nnormal d)
  where
    nnormal = fromJust $ normal n

initSphere :: Vector3 -> Double -> Maybe Shape
initSphere c r
  | r <= 0    = Nothing
  | otherwise = Just (Sphere c r)

initPolygon :: Point3 -> Point3 -> Point3 -> Maybe Shape
initPolygon v0 v1 v2
  | e1 == zero = Nothing
  | e2 == zero = Nothing
  | n  == zero = Nothing
  | otherwise  = Just (Polygon v0 e1 e2 n)
  where
    e1 = v1 - v0
    e2 = v2 - v0
    n = e1 <**> e2

side :: Shape -> Point3 -> Double
side (Plain n d) p = (n <.> p) + d
side (Sphere c r) p = norm (p - c) - r
side (Polygon v e1 e2 n) p = n <.> (p - v)

frontObjects :: Double -> Shape -> Ray -> [PreDistance]
frontObjects err shp ray = [x | x <- (distance shp ray), isFront err x]

distance :: Shape -> Ray -> [PreDistance]
distance shp@(Plain n d) (Ray pos dir)
  | cos == 0  = []
  | otherwise = [((d + n <.> pos) / (-cos), shp, io)]
  where
    cos = n <.> dir
    io  = if cos < 0 then Inside else Outside
distance shp@(Sphere c r) (Ray pos dir)
  | t1 <= 0.0 = []
  | t1 >  0.0 = [(t0 - t2, shp, Inside), (t0 + t2, shp, Outside)]
  where
    o  = c - pos
    t0 = o <.> dir
    t1 = r * r - (square o - (t0 * t0))
    t2 = sqrt t1
distance shp@(Polygon v e1 e2 n) (Ray pos dir)
  | det  == 0        = []
  | beta  < 0        = []
  | gamma < 0        = []
  | beta + gamma > 1 = []
  | otherwise = [(t, shp, io)]
  where
    det   = -(n <.> dir)
    r     = pos - v
    t     = (n <.> r) / det
    beta  = ((dir <**> e2) <.> r) / det
    gamma = ((e1 <**> dir) <.> r) / det
    io    = if det < 0 then Outside else Inside

isFront :: Double -> PreDistance -> Bool
isFront err (t, shp, io) = t >= err

getNormal :: Shape -> Point3 -> Inout -> Direction3
getNormal shp pt i = directNormal (getNormalShape shp pt) i

getNormalShape :: Shape -> Point3 -> Direction3
getNormalShape (Plain n d) pt         = n
getNormalShape (Sphere c r) pt        = fromJust (normal (pt - c))
getNormalShape (Polygon v e1 e2 n) pt = fromJust $ normal n

directNormal :: Vector3 -> Inout -> Vector3
directNormal n io
  | io == Inside = n
  | otherwise    = negate n

{-
intersect' :: Shape -> Ray -> [(Ray, Inout)]
intersect' s@(Plain n _) ray = [(Ray (target ray t) n, io) | (t, shp, io) <- distance s ray]
intersect' s@(Sphere c _) ray = [(newray ray t c, io) | (t, shp, io) <- distance s ray]

newray :: Ray -> Double -> Vector3 -> Ray
newray ray t c = Ray is (is ^- c)
  where is = target ray t
-}

