

--
-- Photon:
--

module Aya.Photon
  (
  ) where

import Aya.Algebra

----------
-- TYPE --
----------

-- Wavelength

data Wavelength = Red | Green | Blue

-- Behaivior

data Behaivior = Diffuse | Specular | Absorbed

-- Photon

data Photon = Photon Wavelength Point3 Direction3

tracePhoton :: Photon -> [Primitive] -> Material -> Int -> [Double] -> [Photon]
tracePhoton ph@(Photon wl p d) prims mat0 depth (x:xs)
  | depth > maxDepth  = []
  | ray == Nothing    = []
  | dt  == Nothing    = []
  | btype == Absorbed = []
  | otherwise         = mark:
  where
    ray = photonToRay ph
    dt  = psearch prims fromJust ray
    dt' = fromJust dt
    (pt, mate1, _) = targetMaterial dt' ray mate0
    btype = selectBehaivior ph mate1 x
    mark = if btype == Diffuse then [(Photon wl pt d)] else []

photonToRay :: Photon -> Maybe Ray
photonToRay (Photon _ p d) = initRay p d

selectBehaivior :: Photon -> Material -> Double -> Behaivior
selectBehaivior ph ray mis mate0 x
  | x < rhod        = Diffuse
  | x < rhod + rhos = Specular
  | otherwise       = Absorbed
  where
    -- mate1 is target Material
    rhod = rho_d mate1
    rhos = rho_s mate1

selectWavelen :: Photon -> Color -> Double
selectWavelen (Photon w _ _) (Color r g b)
  | w == Red   = r
  | w == Green = g
  | w == Blue  = b

