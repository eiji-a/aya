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

data Ray = Ray {rpos :: Vector3, rdir :: Vector3} deriving Show

initRay :: Vector3 -> Vector3 -> Maybe Ray
initRay pos dir
  | ndir == Nothing  = Nothing
  | otherwise        = Just (Ray pos (fromJust ndir))
  where ndir = normal dir
target :: Ray -> Double -> Vector3
target (Ray pos dir) t = pos `add` (dir `scale` t)

-- dir : eye direction
-- n   : normal vector
-- pt  : intersection point
fresnelRay :: Ray -> Vector3 -> Vector3 -> Maybe Ray
fresnelRay (Ray _ dir) n pt = initRay pt (dir `sub` (n `scale` (2 * cos)))
  where cos = dir `dot` n


-- shape class
-----------------------------------------------------------

data CsgOpe = CsgOr | CsgAnd | CsgXor deriving Eq

data Shape = Plain Vector3 Double
           | Sphere Vector3 Double
           | Csg CsgOpe Shape Shape

instance Show Shape where
  show (Plain n d) = "[" ++ (show n) ++ "," ++ (show d) ++ "]"
  show (Sphere c r) = "[" ++ show c ++ "," ++ show r ++ "]"

initPlain :: Vector3 -> Double -> Maybe Shape
initPlain n d
  | n == o3   = Nothing
  | otherwise = Just (Plain (fromJust nnormal) d)
  where nnormal = normal n

initSphere :: Vector3 -> Double -> Maybe Shape
initSphere c r
  | r == 0    = Nothing
  | otherwise = Just (Sphere c r)

side :: Shape -> Vector3 -> Double
side (Plain n d) p = n `dot` p + d
side (Sphere c r) p = norm (p `sub` c) - r

distance :: Shape -> Ray -> [(Double, Shape, Inout)]
distance shp@(Plain n d) (Ray pos dir)
  | cos == 0  = []
  | otherwise = [((d + n `dot` pos) / (-cos), shp, io)]
  where cos = n `dot` dir
        io  = if cos < 0 then Inside else Outside
distance shp@(Sphere c r) (Ray pos dir)
  | t1 <= 0.0 = []
  | t1 >  0.0 = [(t0 - t2, shp, Inside), (t0 + t2, shp, Outside)]
  where o = c `sub` pos
        t0 = o `dot` dir
        t1 = r * r - (square o - (t0 * t0))
        t2 = sqrt t1

dist :: (Double, Shape, Inout) -> Double
dist (t, shp, io) = t

distance' :: Shape -> Ray -> [(Double, Shape, Inout)]
distance' shp ray = [x | x <- (distance shp ray), dist x > 0]

getNormal :: Shape -> Vector3 -> Vector3
getNormal (Plain n d) pt = n
getNormal (Sphere c r) pt = fromJust (normal (pt `sub` c))

getNormal' :: Shape -> Vector3 -> Inout -> Vector3
getNormal' (Plain n d) pt i = directNormal n i
getNormal' (Sphere c r) pt i = directNormal (sphereNormal pt c) i

intersect' :: Shape -> Ray -> [(Ray, Inout)]
intersect' s@(Plain n _) ray = [(Ray (target ray t) n, io) | (t, shp, io) <- distance s ray]
intersect' s@(Sphere c _) ray = [(newray ray t c, io) | (t, shp, io) <- distance s ray]

newray :: Ray -> Double -> Vector3 -> Ray
newray ray t c = Ray is (sphereNormal is c)
  where is = target ray t

directNormal :: Vector3 -> Inout -> Vector3
directNormal n io
  | io == Outside = n
  | otherwise     = neg n

sphereNormal :: Vector3 -> Vector3 -> Vector3
sphereNormal pt c = fromJust (normal (pt `sub` c))

