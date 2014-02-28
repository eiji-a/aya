--
-- InitWorld:
--

module Aya.InitWorld
  ( generateRay
  , offsetXy'
  , eyedir
  , eex
  , eey
  , stepx
  , stepy
  , origin
  ) where

import Data.Maybe

import Aya.Algebra
import Aya.Geometry
import Aya.Scene

-- automatic caluculation

eyedir = fromJust $ normal (etarget ^- eyepos)
eex    = upper ^** eyedir
eey    = eex ^** eyedir
stepx  = (snd xRegion - fst xRegion) / fromIntegral xReso
stepy  = (snd yRegion - fst yRegion) / fromIntegral yReso
origin = (eyedir ^* focus) ^+
         (eex ^* ((fst xRegion) + 0.5 * stepx)) ^-
         (eey ^* ((snd yRegion) + 0.5 * stepy))

generateRay :: (Double, Double) -> Maybe Ray
generateRay (y, x) = initRay eyepos (origin ^+ (eex ^* (stepx * x)) ^+
                                               (eey ^* (stepy * y)))

offsetXy :: (Int, Int) -> (Int, Int) -> Int
offsetXy (y, x) (dy, dx)
  | ofst < 0 = error "Offset is negative 1"
  | otherwise = ofst
  where
    ofst = (y + dy) * xReso + (x + dx)

offsetXy' :: (Int, Int) -> Int
offsetXy' (y, x)
  | ofst < 0 = error "Offset is negative 2"
  | otherwise = ofst
  where
    ofst = y * xReso + x
