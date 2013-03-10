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

instance Raytrace Tracer where
  trace (Tracer lgts prims) ray depth
    | depth <= 0 = intensityBlack
    | otherwise  = localdiff `iadd` localspec `iadd` globalspec `iadd` globaltran

psearch :: [Primitive] -> Ray -> Maybe Intersection
psearch prims ray = nearest [y | y <- concat [intersect x | x <- prims], idist y > 0.0]

nearest :: [Intersection] -> Maybe Intersection
nearest [] = Nothing
nearest (t:ts) = Just (minp t (nearest ts))

minp :: Intersection -> Intersection -> Intersection
minp t1 t2
  | t2 == Nothing        = t1
  | idist t1 <= idist t2 = t1
  | otherwise            = t2

