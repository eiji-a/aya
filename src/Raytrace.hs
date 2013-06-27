--
-- RAYTRACE
--

module Raytrace where

import Algebra
import Geometry
import Physics

class Raytrace a where
  trace :: a -> Ray -> Material -> Int -> Intensity

data Tracer = Tracer [Light] [Primitive]

instance Raytrace Tracer where
  trace tr@(Tracer lgts prims) ray mate depth
    | depth <= 0    = intensityBlack
    | is == Nothing = intensityBlack
    | otherwise     = brightnessDiff mate1 tr ray pt n
                      `iadd` trace tr rs mate (depth - 1)
                      `iadd` intensityBlack
                      -- trace t rt mate1 (depth - 1)
                      `iadd` iAmb
    where is    = psearch prims ray                        -- intersection info
          pt    = target ray (isdist is)                   -- intersection point
          n     = getNormal' (isshape is) pt (inout is)    -- normal vector on pt
          mate1 = ismate is                                -- material
          rs    = fresnel n ray pt
          -- rt = Ray pt ey3

psearch :: [Primitive] -> Ray -> Maybe Intersection
psearch prims ray
  | iss == [] = Nothing
  | otherwise = Just (minimum iss)
  where iss = concat [intersect x ray | x <- prims]

tracePoint :: (Int, Int) -> Rgb
tracePoint (y, x) = Rgb x y 0

