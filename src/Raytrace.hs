--
-- RAYTRACE
--

module Raytrace where

import Data.Maybe

import Algebra
import Geometry
import Physics
import Object
import Scene

class Raytrace a where
  trace :: a -> Maybe Ray -> Material -> Int -> Intensity

data Tracer = Tracer [Light] [Primitive]

instance Raytrace Tracer where
  trace _ _ _ 0       = intensityBlack
  trace _ Nothing _ _ = intensityBlack
  trace tr@(Tracer lgts prims) (Just ray) mate depth
    | mis == Nothing = intensityBlack
    | otherwise      = (brightnessDiff lgts prims is) `iadd`
                       (calcSpec is tr mate (depth - 1))
    where mis   = psearch prims ray
          is    = initIntersection mis ray
{--
    | otherwise     = brightnessDiff mate1 tr ray pt n
                      `iadd` trace tr rs mate (depth - 1)
                      -- trace t rt mate1 (depth - 1)
          rs    = fresnel n ray pt
          -- rt = Ray pt ey3
--}

psearch :: [Primitive] -> Ray -> Maybe Intersection'
psearch prims ray
  | iss == [] = Nothing
  | otherwise = Just (minimum iss)
  where iss = concat [intersect x ray | x <- prims]

brightnessDiff :: [Light] -> [Primitive] -> Intersection -> Intensity
brightnessDiff lgts prims is = (mdiff $ ismate is) `imul` addLight lgts prims is

addLight :: [Light] -> [Primitive] -> Intersection -> Intensity
addLight [] _ _ = intensityBlack
addLight (l:ls) prims is = (getLightIntensity l prims is) `iadd` (addLight ls prims is)

getLightIntensity :: Light -> [Primitive] -> Intersection -> Intensity
getLightIntensity lgt prims is
  | mis == Nothing = lintensity `iadd` hlgt
  | otherwise      = intensityBlack
  where (ld, decay) = ldir lgt (ispt is)
        mis         = psearch prims (fromJust $ initRay (ispt is) (fromJust ld))
        lintensity  = (lint lgt) `iscale` ((fromJust ld `dot` isn is) / decay)
        hlgt        = (lint lgt) `iscale` (calcHighlight is (fromJust ld))

calcHighlight :: Intersection -> Vector3 -> Double
calcHighlight is ld
  | hvec == Nothing = 0
  | otherwise       = (fromJust hvec `dot` (isn is)) ^ 200
  where hvec = normal (ld `sub` (isedir is))

calcSpec :: Intersection -> Tracer -> Material -> Int -> Intensity
calcSpec is tr mate0 depth
  | spec == intensityBlack = intensityBlack
  | otherwise = (trace tr (isrray is) mate0 depth) `imul` spec
  where spec = mspec $ ismate is
