--
-- RAYTRACE
--

module Raytrace where

import Algebra
import Geometry
import Physics

class Raytrace a where
  trace :: a -> Ray -> Int -> Intensity

data Tracer = Tracer [Light] [Primitive]

<<<<<<< HEAD
instance Raytrace Tracer where
  trace (Tracer lgts prims) ray depth
    | depth <= 0 = intensityBlack
    | otherwise  = localdiff `iadd` localspec `iadd` globalspec `iadd` globaltran

psearch :: [Primitive] -> Ray -> Maybe Intersection
psearch prims ray = nearest [y | y <- concat [intersect x | x <- prims], idist y > 0.0]
=======
trace t@(Tracer l p) r lv
  | lv <= 0       = intensityBlack
  | is == Nothing = intensityBlack
  | otherwise     = brightnessDiff m t r pt n `iadd`
                    trace t rs (lv - 1) `iadd` intensityBlack
                    -- trace t rt (lv - 1)
  where is = psearch p r                              -- intersection info
        pt = target r (isdist is)                     -- intersection point
        n  = getNormal' (isshape is) pt (inout is)    -- normal vector on pt
        m  = ismate is                                -- material
        rs = fresnel n r pt
        -- rt = Ray pt ey3

psearch :: [Primitive] -> Ray -> Maybe Intersection
psearch p r = nearest [y | y <- concat [intersect x r | x <- p], isdist y > 0]
>>>>>>> update 2013/3/9 mac

nearest :: [Intersection] -> Maybe Intersection
nearest [] = Nothing
nearest (t:ts) = Just (minp t (nearest ts))

minp :: Intersection -> Intersection -> Intersection
minp is1 is2
  | is2 == Nothing           = is1
  | isdist is1 <= isdist is2 = is1
  | otherwise                = is2


