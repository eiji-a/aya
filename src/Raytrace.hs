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
  trace tr@(Tracer lgts prims) ray depth
    | depth <= 0    = intensityBlack
    | is == Nothing = intensityBlack
    | otherwise     = brightnessDiff mate tr ray pt n
                      `iadd` trace tr rs (depth - 1)
                      `iadd` intensityBlack
                      -- trace t rt (depth - 1)
    where is   = psearch p ray                            -- intersection info
          pt   = target ray (isdist is)                   -- intersection point
          n    = getNormal' (isshape is) pt (inout is)    -- normal vector on pt
          mate = ismate is                                -- material
          rs   = fresnel n ray pt
          -- rt = Ray pt ey3

psearch :: [Primitive] -> Ray -> Maybe Intersection
psearch prims ray = nearest [y | y <- concat [intersect x ray | x <- prims], isdist y > 0]

nearest :: [Intersection] -> Maybe Intersection
nearest [] = Nothing
nearest (t:ts) = Just (minp t (nearest ts))

minp :: Intersection -> Intersection -> Intersection
minp is1 is2
  | is2 == Nothing           = is1
  | isdist is1 <= isdist is2 = is1
  | otherwise                = is2


