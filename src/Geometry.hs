--
-- GEOMETRY
--

module Geometry where

import Data.Maybe
import Algebra

-- inside/outside
-----------------------------------------------------------

data Inout = Inside | Outside deriving Eq

instance Show Inout where
  show a
    | a == Inside  = "Inside"
    | a == Outside = "Outside"

-- ray 
----------------------------------------------------------

data Ray = Ray {rpos :: Point3, rdir :: Direction3} deriving Show

instance Eq Ray where
  (==) a b = (rpos a == rpos b) && (rdir a == rdir b)

initRay :: Point3 -> Direction3 -> Maybe Ray
initRay pos dir
  | ndir == Nothing  = Nothing
  | otherwise        = Just $ Ray pos $ fromJust ndir
  where ndir = normal dir

target :: Ray -> Double -> Vector3
target (Ray pos dir) t = pos ^+ (dir ^* t)

-- shape class
-----------------------------------------------------------


data CsgOpe = CsgOr | CsgAnd | CsgXor deriving Eq
data Shape = Plain Vector3 Double
           | Sphere Vector3 Double
           | Csg CsgOpe Shape Shape
type Distance = (Double, Shape, Inout)

instance Eq Shape where
  (==) (Plain n1 d1) (Plain n2 d2) = (n1 == n2) && (d1 == d2)
  (==) (Sphere c1 r1) (Sphere c2 r2) = (c1 == c2) && (r1 == r2)

instance Show Shape where
  show (Plain n d) = "[" ++ (show n) ++ "," ++ (show d) ++ "]"
  show (Sphere c r) = "[" ++ show c ++ "," ++ show r ++ "]"

initPlain :: Vector3 -> Double -> Maybe Shape
initPlain n d
  | n == o3   = Nothing
  | otherwise = Just (Plain nnormal d)
  where nnormal = fromJust $ normal n

initSphere :: Vector3 -> Double -> Maybe Shape
initSphere c r
  | r <= 0    = Nothing
  | otherwise = Just (Sphere c r)

side :: Shape -> Vector3 -> Double
side (Plain n d) p = (n ^. p) + d
side (Sphere c r) p = norm (p ^- c) - r

distance :: Shape -> Ray -> [Distance]
distance shp@(Plain n d) (Ray pos dir)
  | cos == 0  = []
  | otherwise = [((d + n ^. pos) / (-cos), shp, io)]
  where cos = n ^. dir
        io  = if cos < 0 then Inside else Outside
distance shp@(Sphere c r) (Ray pos dir)
  | t1 <= 0.0 = []
  | t1 >  0.0 = [(t0 - t2, shp, Inside), (t0 + t2, shp, Outside)]
  where o = c ^- pos
        t0 = o ^. dir
        t1 = r * r - (square o - (t0 * t0))
        t2 = sqrt t1

dist :: Distance -> Double
dist (t, shp, io) = t

distance' :: Shape -> Ray -> [Distance]
distance' shp ray = [x | x <- (distance shp ray), dist x >= 0.02]
--distance' shp ray = [x | x <- (distance shp ray), dist x >= (-0.001)]

getNormal :: Shape -> Vector3 -> Vector3
getNormal (Plain n d) pt = n
getNormal (Sphere c r) pt = fromJust (normal (pt ^- c))

getNormal' :: Shape -> Vector3 -> Inout -> Vector3
getNormal' shp pt i = directNormal (getNormal shp pt) i

intersect' :: Shape -> Ray -> [(Ray, Inout)]
intersect' s@(Plain n _) ray = [(Ray (target ray t) n, io) | (t, shp, io) <- distance s ray]
intersect' s@(Sphere c _) ray = [(newray ray t c, io) | (t, shp, io) <- distance s ray]

newray :: Ray -> Double -> Vector3 -> Ray
newray ray t c = Ray is (is ^- c)
  where is = target ray t

directNormal :: Vector3 -> Inout -> Vector3
directNormal n io
  | io == Inside = n
  | otherwise    = neg n
