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

-- shape class
-----------------------------------------------------------

data Shape = Plain Vector3 Double
           | Sphere Vector3 Double

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

distance :: Shape -> Ray -> [(Double, Inout)]
distance (Plain n d) (Ray pos dir)
  | cos == 0.0 = []
  | otherwise  = [((d + n `dot` pos) / (-cos), io)]
  where cos = n `dot` dir
        io  = if cos < 0 then Inside else Outside
distance (Sphere c r) (Ray pos dir)
  | t1 <= 0.0 = []
  | t1 >  0.0 = [(t0 - t2, Inside), (t0 + t2, Outside)]
  where o = c `sub` pos
        t0 = o `dot` dir
        t1 = r * r - (square o - (t0 * t0))
        t2 = sqrt t1

getNormal :: Shape -> Vector3 -> Vector3
getNormal (Plain n d) pt = n
getNormal (Sphere c r) pt = fromJust (normal (pt `sub` c))

intersect' :: Shape -> Ray -> [(Ray, Inout)]
intersect' s@(Plain n _) ray = [(Ray (target ray t) n, io) | (t, io) <- distance s ray]
intersect' s@(Sphere c _) ray = [(newray ray t c, io) | (t, io) <- distance s ray]

newray :: Ray -> Double -> Vector3 -> Ray
newray ray t c = Ray is n
  where is = target ray t
        n = fromJust (normal (is `sub` c))

