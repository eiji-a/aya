--
--
--

module Geometry where

import Algebra

-- ray 
----------------------------------------------------------

data Ray = Ray Vector3 Vector3 deriving (Show)

init_ray :: Vector3 -> Vector3 -> Maybe Ray
init_ray p d =
  | nm == Nothing = Nothing
  | otherwise     = Ray p nm
  where nm = normal d

pos :: Ray -> Vector3
pos (Ray p d) = p

dir :: Ray -> Vector3
dir (Ray p d) = d

target :: Ray -> Double -> Vector3
target (Ray p d) t = p `add` (d `scale` t)

-- shape class
-----------------------------------------------------------

data Shape = Plain Vector3 Double |
             Sphere Vector3 Double

init_plain :: Vector3 -> Double -> Maybe Plain
init_plain n d =
  | n == o3   = Nothing
  | otherwise = Just (Plain nm d)
  where nm = normal n

init_sphere :: Vector3 -> Double -> Maybe Sphere
init_sphere c r =
  | r == 0 = Nothing
  | otherwise = Just (Sphere c r)

instance Show Shape where
  show (Plain n d) = "[" ++ (show n) ++ "," ++ (show d) ++ "]"
  show (Sphere c r) = "[" ++ show c ++ "," ++ show r ++ "]"

side :: Shape -> Vector3 -> Double
side (Plain n d) p = n `dot` p + d
side (Sphere c r) p = norm (p `sub` c) - r

distance :: Shape -> Ray -> [(Double, Bool)]
distance (Plain n d) (Ray p dr)
  | c == 0.0 = []
  | otherwise = [((d + n `dot` p) / (-c), c < 0.0)]
  where c = n `dot` dr
distance (Sphere c r) (Ray p d)
  | t1 <= 0.0 = []
--  | t1 == 0.0 = [t0]
  | t1 >  0.0 = [(t0 - t2, True), (t0 + t2, False)]
  where o = c `sub` p
        t0 = o `dot` d
        t1 = r * r - (square o - (t0 * t0))
        t2 = sqrt t1

get_normal :: Shape -> Vector3 -> Vector3
get_normal (Plain n d) p = n
get_normal (Sphere c r) p = normal (p `sub` c)

intersect' :: Shape -> Ray -> [(Ray, Bool)]
intersect' (Plain n d) ray@(Ray p dr)
  | c == 0.0 = []
  | otherwise = [(Ray pos n, c < 0.0)]
  where c = n `dot` dr
        pos = target ray ((d + n `dot` p) / (-c))
intersect' (Sphere c r) ray@(Ray p d)
  | t1 <= 0.0 = []
--  | t1 == 0.0 = [(Ray t0 (noral (p1 `sub` c)), True)]
  | t1 >  0.0 = [(init_ray p1 (p1 `sub` c), True),
                 (init_ray p2 (p2 `sub` c), False)]
  where o = c `sub` p
        t0 = o `dot` d
        t1 = r * r - (square o - (t0 * t0))
        t2 = sqrt t1
        p1 = target ray (t0 - t2)
        p2 = target ray (t0 + t2)
