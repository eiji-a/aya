--
-- RAYTRACE
--

module Raytrace
  ( Tracer(..)
  , trace
  ) where

import Data.Maybe

import Algebra
import Geometry
import Physics
import Object
import Scene

class Raytrace a where
  trace :: a -> Maybe Ray -> Material -> Int -> Intensity

data Tracer = Tracer [Light] [Primitive]
data TraceDirection = Speculum | Transparence deriving Eq

instance Raytrace Tracer where
  trace _ _ _ 0       = intensityBlack   -- achive max trace depth
  trace _ Nothing _ _ = intensityBlack   -- no ray for trace
  trace tr@(Tracer lgts prims) (Just ray) mate depth
    | mis == Nothing = intensityBlack
    | otherwise      =  (brightnessDiff lgts prims is)
                     !+ (calcTrace Speculum     is tr (depth - 1))
                     !+ (calcTrace Transparence is tr (depth - 1))
    where mis   = psearch prims ray
          is    = initIntersection mis ray material0 mate

psearch :: [Primitive] -> Ray -> Maybe Distance
psearch prims ray
  | iss == [] = Nothing
  | otherwise = Just (minimum iss)
  where iss = concat [intersect x ray | x <- prims]

brightnessDiff :: [Light] -> [Primitive] -> Intersection -> Intensity
brightnessDiff lgts prims is = (mdiff $ ismate2 is) !** addLight lgts prims is

addLight :: [Light] -> [Primitive] -> Intersection -> Intensity
addLight [] _ _ = intensityBlack
addLight (l:ls) prims is = (getLightIntensity l prims is) !+ (addLight ls prims is)

getLightIntensity :: Light -> [Primitive] -> Intersection -> Intensity
getLightIntensity lgt prims is
  | mis == Nothing = lintensity !+ hlgt
  | otherwise      = intensityBlack
  where (ld, decay) = ldir lgt (ispt is)
        mis         = psearch prims $ fromJust $ initRay (ispt is) $ fromJust ld
        lintensity  = (lint lgt) !* ((fromJust ld ^. isn is) / decay)
        hlgt        = (lint lgt) !* (calcHighlight is (fromJust ld))

calcHighlight :: Intersection -> Vector3 -> Double
calcHighlight is ld
  | hvec == Nothing = 0
  | otherwise       = (fromJust hvec ^. (isn is)) ^ 200
  where hvec = normal (ld ^- (isedir is))

calcTrace :: TraceDirection -> Intersection -> Tracer -> Int -> Intensity
calcTrace td is tr depth
  | kp  == 0.0     = intensityBlack
  | ray == Nothing = intensityBlack
  | otherwise      = (trace tr ray mate depth) !** (mcolor !* kp)
  where (kp, ray, mate, mcolor) = getTraceParam td is

getTraceParam :: TraceDirection -> Intersection -> (Double, Maybe Ray, Material, Intensity)
getTraceParam td is
  | td == Speculum     = (iskr is, isrray is, ismate1 is, mspec $ ismate2 is)
  | td == Transparence = (iskt is, istray is, ismate2 is, mtran $ ismate2 is)
