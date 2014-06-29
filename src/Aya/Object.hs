{-# LANGUAGE NoImplicitPrelude #-}

--
-- Object:
--

module Aya.Object
  ( Light(..)
  , ldir
  , lint
  , isLight
  , Primitive(..)
  , intersect
  , Distance
  , dtdist
  , dtshape
  , targetMaterial
  , Intersection
  , initIntersection
  , ispt
  , isn
  , isedir
  , iskr
  , iskt
  , isrray
  , istray
  , ismate1
  , ismate2
  , ismate
  ) where

import Data.Maybe
import NumericPrelude

import Aya.Algebra
import Aya.Color
import Aya.Geometry
import Aya.Mapping
import Aya.Material

--
-- light
--

data Light =
    PointLight Vector3 Intensity Primitive
  | DirectiveLight Vector3 Intensity Vector3 Primitive
  | ParallelLight Vector3 Intensity

type LightDirection = (Maybe Vector3, Double)
-- return: L vector and decay factor
ldir :: Light -> Vector3 -> LightDirection
ldir (ParallelLight dir _) _ = (Just dir, 1.0)
ldir (PointLight pos _ _) pt = ldir' pos pt
ldir (DirectiveLight pos _ _ _) pt = ldir' pos pt

ldir' :: Vector3 -> Vector3 -> LightDirection
ldir' pos pt
  | len2 == 0 = (Nothing, 0)
  | otherwise = (ld </> len, len2)
  where
    ld = pos - pt
    len2 = square ld
    len = sqrt len2

lint :: Light -> Vector3 -> Intensity
lint (ParallelLight _ i) _ = i
lint (PointLight _ i _) _ = i
lint (DirectiveLight _ i d _) ldir = i <*> cos
  where
    cos' = -(d <.> ldir)
    cos  = if cos' < 0 then 0 else cos'

isLight :: Light -> Shape -> Bool
isLight (ParallelLight _ _) s = False
isLight (PointLight _ _ (Primitive ls _)) s = (ls == s)
isLight (DirectiveLight _ _ _ (Primitive ls _)) s = (ls == s)

--
-- Primitive
--

-- intersection
---------------

data Distance = Distance {
    dtdist  :: Double
  , dtshape :: Shape
  , dtmap   :: MapFunc
  , inout   :: Inout
  }

instance Eq Distance where
  (==) dt dt' = (dtdist dt) == (dtdist dt')

instance Ord Distance where
  compare dt dt'
    | dtdist dt == dtdist dt' = EQ
    | dtdist dt <= dtdist dt' = LT
    | otherwise               = GT

targetMaterial :: Distance -> Ray -> Material -> (Point3, Material, Material)
targetMaterial dt ray mate0 = (pt, mateT, mate2)
  where
    pt = target ray (dtdist dt)
    mateT = (dtmap dt) pt
    mate2 = if (inout dt) == Inside then mateT else mate0

-- primitive
------------

data Primitive = Primitive Shape MapFunc

intersect :: Primitive -> Ray -> [Distance]
intersect (Primitive shp mapf) ray = map mkIs' (frontObjects 0.02 shp ray)
  where
    mkIs' = mkIs mapf

mkIs :: MapFunc -> PreDistance -> Distance
mkIs mapf (t, shp, io) = Distance t shp mapf io

data Intersection = Intersection
  { ismate1 :: Material
  , ismate2 :: Material
  , ismate  :: Material
  , ispt    :: Vector3
  , isn     :: Vector3
  , isedir  :: Vector3
  , isrray  :: Maybe Ray
  , istray  :: Maybe Ray
  , iskr    :: Double
  , iskt    :: Double
  } deriving Show

initIntersection :: Maybe Distance -> Ray -> Material -> Material
                 -> Intersection
initIntersection dt ray mate0 mate1 =
  Intersection mate1 mate2 mateT pt n edir rray tray kr kt
  where
    dt' = fromJust dt
    (pt, mateT, mate2) = targetMaterial dt' ray mate0
    n  = getNormal (dtshape dt') pt (inout dt')
    cos1  = -(n <.> edir)
    edir = rdir ray
    rray = initRay pt ((n <*> (2 * cos1)) + edir)
    (tray, kr, kt) = fresnel pt edir n cos1 (avgeta mate1) (avgeta mate2)

