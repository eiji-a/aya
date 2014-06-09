{-# LANGUAGE NoImplicitPrelude #-}

--
-- Raytrace:
--

module Aya.Raytrace
  ( Tracer(..)
  , trace
  ) where

import Data.Maybe
import NumericPrelude

import Aya.Algebra
import Aya.Color
import Aya.Geometry
import Aya.Material
import Aya.Object
import Aya.Scene

class Raytrace a where
  trace :: a -> Maybe Ray -> Material -> Int -> Intensity

data Tracer = Tracer [Light] [Primitive]
data TraceDirection = Speculum | Transparence deriving Eq

instance Raytrace Tracer where
  trace _ _ _ 0       = intensityZero   -- achive max trace depth
  trace _ Nothing _ _ = intensityZero   -- no ray for trace
  trace tr@(Tracer lgts prims) (Just ray) mate depth
    | mis == Nothing = intensityZero
    | otherwise      = (brightnessDiff lgts prims is)
                     + (calcTrace Speculum     is tr (depth - 1))
                     + (calcTrace Transparence is tr (depth - 1))
                     + ra
    where
      mis = psearch prims ray
      is  = initIntersection mis ray material0 mate
      ra  = radi $ ismate2 is

psearch :: [Primitive] -> Ray -> Maybe Distance
psearch prims ray
  | iss == [] = Nothing
  | otherwise = Just (minimum iss)
  where
    iss = concat [intersect x ray | x <- prims]

brightnessDiff :: [Light] -> [Primitive] -> Intersection -> Intensity
brightnessDiff lgts prims is = (addLight lgts prims is) !** (rho_d $ ismate is)

addLight :: [Light] -> [Primitive] -> Intersection -> Intensity
addLight [] _ _          = intensityZero
addLight (l:ls) prims is = (getLightIntensity l prims is)
                         + (addLight ls prims is)

getLightIntensity :: Light -> [Primitive] -> Intersection -> Intensity
getLightIntensity lgt prims is
  | mis == Nothing      = lintensity + hlgt
  | lightprim == True   = lintensity + hlgt
  | dist * dist > decay = lintensity + hlgt
  | otherwise           = intensityZero
  where
    (ld, decay) = ldir lgt (ispt is)
    mis         = psearch prims $ fromJust $ initRay (ispt is) $ fromJust ld
    lightprim   = isLight lgt (dtshape $ fromJust mis)
    dist        = dtdist $ fromJust mis
    ld'         = fromJust ld
    lintensity  = (lint lgt ld') <*> ((ld' <.> isn is) / decay)
    hlgt        = intensityZero
    --hlgt        = (lint lgt) <*> (calcHighlight is (fromJust ld))

calcTrace :: TraceDirection -> Intersection -> Tracer -> Int -> Intensity
calcTrace td is tr depth
  | kp  == 0.0     = intensityZero
  | ray == Nothing = intensityZero
  | otherwise      = (trace tr ray mate depth) !** (mcolor <*> kp)
  where
    (kp, ray, mate, mcolor) = getTraceParam td is

getTraceParam :: TraceDirection -> Intersection
              -> (Double, Maybe Ray, Material, Color)
getTraceParam td is
  | td == Speculum     = (iskr is, isrray is, ismate1 is, rho_s $ ismate2 is)
  | td == Transparence = (iskt is, istray is, ismate2 is, tr $ ismate2 is)

calcHighlight :: Intersection -> Vector3 -> Double
calcHighlight is ld
  | hvec == Nothing = 0
  | otherwise       = (fromJust hvec <.> (isn is)) ^ 1000
  where
    hvec = normal (ld - (isedir is))
