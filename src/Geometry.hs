--
-- GEOMETRY
--

module Geometry where

import Data.Maybe
import Algebra

-- ray 
----------------------------------------------------------

data Ray = Ray {rpos :: Vector3, rdir :: Vector3} deriving Show

initRay :: Vector3 -> Vector3 -> Maybe Ray
initRay p d
  | nm == Nothing  = Nothing
  | otherwise      = Just (Ray p (fromJust nm))
  where nm = normal d

target :: Ray -> Double -> Vector3
target (Ray p d) t = p `add` (d `scale` t)

-- inside/outside
-----------------------------------------------------------

data Inout = Inside | Outside deriving Eq

instance Show Inout where
  show a
    | a == Inside  = "Inside"
    | a == Outside = "Outside"

-- shape class
-----------------------------------------------------------

data Shape = Plain Vector3 Double |
             Sphere Vector3 Double

initPlain :: Vector3 -> Double -> Maybe Shape
initPlain n d
  | n == o3   = Nothing
  | otherwise = Just (Plain (fromJust nm) d)
  where nm = normal n

initSphere :: Vector3 -> Double -> Maybe Shape
initSphere c r
  | r == 0 = Nothing
  | otherwise = Just (Sphere c r)

instance Show Shape where
  show (Plain n d) = "[" ++ (show n) ++ "," ++ (show d) ++ "]"
  show (Sphere c r) = "[" ++ show c ++ "," ++ show r ++ "]"

side :: Shape -> Vector3 -> Double
side (Plain n d) p = n `dot` p + d
side (Sphere c r) p = norm (p `sub` c) - r

distance :: Shape -> Ray -> [(Double, Inout)]
distance (Plain n d) (Ray p dr)
  | c == 0.0 = []
  | otherwise = [((d + n `dot` p) / (-c), io)]
  where c = n `dot` dr
        io = if c < 0.0 then Inside else Outside
distance (Sphere c r) (Ray p d)
  | t1 <= 0.0 = []
--  | t1 == 0.0 = [t0]
  | t1 >  0.0 = [(t0 - t2, Inside), (t0 + t2, Outside)]
  where o = c `sub` p
        t0 = o `dot` d
        t1 = r * r - (square o - (t0 * t0))
        t2 = sqrt t1

getNormal :: Shape -> Vector3 -> Vector3
getNormal (Plain n d) p = n
getNormal (Sphere c r) p = fromJust (normal (p `sub` c))

intersect' :: Shape -> Ray -> [(Ray, Inout)]
intersect' s@(Plain n _) r = [(Ray (target r t) n, io) | (t, io) <- distance s r]
intersect' s@(Sphere c _) r = [(newray r t c, io) | (t, io) <- distance s r]

newray :: Ray -> Double -> Vector3 -> Ray
newray r t c = Ray is n
  where is = target r t
        n = fromJust (normal (is `sub` c))

