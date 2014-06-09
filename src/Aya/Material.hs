{-# LANGUAGE NoImplicitPrelude #-}

--
-- Material:
--

module Aya.Material
  ( Material(..)
  , fresnel
  ) where

import Data.Maybe
import NumericPrelude

import Aya.Algebra
import Aya.Color
import Aya.Geometry

--
-- Material
--

-- material
-----------

data Material = Material {
    radi   :: Intensity     -- 放射輝度(radiance)
  , rho_d  :: Color         -- 拡散反射率(0<ρd<1)
  , rho_s  :: Color         -- 鏡面反射率(0<ρs<1)  (0<ρd+ρs<1)
  , tr     :: Color         -- 透明度(0<Tr<1)
  , eta    :: Color         -- 屈折率(η)
  , avgeta :: Double        -- 屈折率平均
  , alpha  :: Double        -- 平滑度(0<α<1)
  } deriving Show

--
-- fresnel
--
-- pt : point of intersection
-- e  : eye direction
-- n  : normal vector
-- cos1 : inner product of e and n
-- eta1 : refractive index of current object
-- eta2 : refractive index of front object

type Catadioptric = (Maybe Ray, Double, Double)  -- 反射屈折情報

fresnel :: Point3 -> Direction3 -> Direction3 -> Double -> Double -> Double -> Catadioptric
fresnel pt e n cos1 eta1 eta2
  | eta1 == 0       = (Nothing, 1.0, 0.0)
  | eta2 == 0       = (Nothing, 1.0, 0.0)
  | g2 < 0          = (Nothing, 1.0, 0.0)
  | tdir == Nothing = (Nothing, 1.0, 0.0)
  | otherwise       = (tray, kr, kt)
  where
    eta  = eta2 / eta1
    g2   = eta * eta + cos1 * cos1 - 1
    g    = sqrt g2
    tdir = (e + (n <*> (cos1 - g))) </> eta
    tray = initRay pt $ fromJust tdir
    gp   = g + cos1
    gm   = g - cos1
    r1   = gm / gp
    r2   = (cos1 * gp - 1) / (cos1 * gm + 1)
    kr   = r1 * r1 * (1 + r2 * r2) / 2
--  n'   = (eta - 1) / (eta + 1)
--  r0   = n' * n'
--  kr = r0 + (1 - r0) * ((1 - cos1) ^ 5)
    kt   = 1 - kr

