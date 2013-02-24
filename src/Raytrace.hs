--
--
--

module Raytrace where

import Algebra
import Geometry
import Physics

class Raytrace a where
  trace :: a -> Ray -> Intensity

data Tracer = Tracer [Light] [Primitive]

trace (Tracer l p) r = localdiff `iadd` localspec `iadd`
                       globalspec `iadd` globaltran
  where 


psearch :: [Primitive] -> Ray -> Intersection
psearch p r = nearest [y | y <- concat [intersect x | x <- p], idist y > 0.0]

nearest :: [Intersection] -> Intersection
nearest [] = Nothing
nearest (t:ts) = minp t (minp ts)

minp :: Intersection -> Intersection -> Intersection
minp t1 t2 = if t2 == Nothing || idist t1 <= idist t2 then t1 else t2

