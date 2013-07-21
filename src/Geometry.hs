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

instance Eq Ray where
  (==) a b = (rpos a == rpos b) && (rdir a == rdir b)

initRay :: Vector3 -> Vector3 -> Maybe Ray
initRay pos dir
  | ndir == Nothing  = Nothing
  | otherwise        = Just (Ray pos (fromJust ndir))
  where ndir = normal dir
target :: Ray -> Double -> Vector3
target (Ray pos dir) t = pos `add` (dir `scale` t)

--
-- fresnel
--
-- pt : point of intersection
-- e  : eye direction
-- n  : normal vector
-- cos1 : inner product of e and n
-- eta1 : refractive index of current object
-- eta2 : refractive index of front object

fresnel :: Vector3 -> Vector3 -> Vector3 -> Double -> Double -> Double -> (Maybe Ray, Double, Double)
fresnel pt e n cos1 eta1 eta2
  | eta1 == 0 = (Nothing, 1.0, 0.0)
  | eta2 == 0 = (Nothing, 1.0, 0.0)
  | g2 < 0    = (Nothing, 1.0, 0.0)
  | tdir == Nothing = (Nothing, 1.0, 0.0)
  | otherwise = (tray, kr, kt)
  where eta = eta2 / eta1
        g2 = eta * eta + cos1 * cos1 - 1
        g  = sqrt g2
        tdir = (e `add` (n `scale` (cos1 - g))) `divide` eta
        tray = initRay pt (fromJust tdir)
        n' = (eta - 1) / (eta + 1)
        r0 = n' * n'
        kr = r0 + (1 - r0) * ((1 - cos1) ^ 5)
        kt = 1 - kr

-- shape class
-----------------------------------------------------------

data CsgOpe = CsgOr | CsgAnd | CsgXor deriving Eq

data Shape = Plain Vector3 Double
           | Sphere Vector3 Double
           | Csg CsgOpe Shape Shape

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
distance' shp ray = [x | x <- (distance shp ray), dist x >= 0.02]
--distance' shp ray = [x | x <- (distance shp ray), dist x >= (-0.001)]

getNormal :: Shape -> Vector3 -> Vector3
getNormal (Plain n d) pt = n
getNormal (Sphere c r) pt = fromJust (normal (pt `sub` c))

getNormal' :: Shape -> Vector3 -> Inout -> Vector3
getNormal' shp pt i = directNormal (getNormal shp pt) i

intersect' :: Shape -> Ray -> [(Ray, Inout)]
intersect' s@(Plain n _) ray = [(Ray (target ray t) n, io) | (t, shp, io) <- distance s ray]
intersect' s@(Sphere c _) ray = [(newray ray t c, io) | (t, shp, io) <- distance s ray]

newray :: Ray -> Double -> Vector3 -> Ray
newray ray t c = Ray is (is `sub` c)
  where is = target ray t

directNormal :: Vector3 -> Inout -> Vector3
directNormal n io
  | io == Inside = n
  | otherwise    = neg n
