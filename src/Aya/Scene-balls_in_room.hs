--
-- SCENE
--

module Aya.Scene
  ( smooth
  , iclip
  , traceDepth
  , xReso
  , yReso
  , xRegion
  , yRegion
  , eyepos
  , etarget
  , upper
  , focus
  , material0
  , iAmb
  , lights
  , primitives
  ) where

import Data.Maybe

import Aya.Algebra
import Aya.Color
import Aya.Geometry
import Aya.Mapping
import Aya.Material
import Aya.Object

-- global parameters

smooth = True
iclip = 120 :: Double

traceDepth = 6 :: Int

xReso = 512 :: Int
yReso = 512 :: Int

xRegion = (-1.0, 1.0)
yRegion = (-1.0, 1.0)

eyepos = Vector3 0 2 (-30)
etarget = Vector3 0 2 0
upper = Vector3 0 1 0
focus = 2.0


material0 = Material intensityZero colorBlack colorBlack colorWhite colorWhite 1 0

iAmb = initIntensity colorWhite 100

-- lights and primitives

lights :: [Light]
lights =
  [ 
    -- PointLight (Vector3 0 5.95 (-10)) (initIntensity colorWhite 5000) lp1
    DirectiveLight (Vector3 0 7.95 (-12)) (initIntensity colorWhite 8000) (neg ey3) lp1
  ]

lp1 = Primitive (fromJust $ initPolygon lpt1 lpt2 lpt3) mplight

primitives :: [Primitive]
primitives =
  [ lp1
  , Primitive (fromJust $ initPolygon lpt3 lpt4 lpt1) mplight
  , Primitive (fromJust $ initPlain ey3 6) mfloor
  , Primitive (fromJust $ initPlain (neg ey3) 8) mfloor
  , Primitive (fromJust $ initPlain (neg ez3) 0) mfloor
  , Primitive (fromJust $ initPlain ex3 8) mrwall
  , Primitive (fromJust $ initPlain (neg ex3) 8) mbwall
  , Primitive (fromJust $ initSphere (Vector3 (-3.5) (-3.5) (-6)) 2.5) mmirr
  , Primitive (fromJust $ initSphere (Vector3 (4.0) (-3.5) (-10)) 2.5) mtrans
  ]

-----
-- details of primitives
-----

plight = Material (initIntensity colorWhite 8000) colorBlack colorBlack colorBlack colorBlack 0 0
ceil = Material intensityZero colorBlack colorBlack colorBlack colorBlack 0 0
floor1 = Material intensityZero (Color 1 1 0.85) colorBlack colorBlack colorBlack 0 0
floor2 = Material intensityZero (Color 1.0 0.4 0.2) colorBlack colorBlack colorBlack 0 0
floor3 = Material intensityZero (Color 0.6 0.5 1) colorBlack colorBlack colorBlack 0 0
ball1 =  Material intensityZero colorBlack colorWhite colorBlack colorBlack 0 0
ball2 =  Material intensityZero colorBlack colorWhite colorWhite (Color 1.51 1.51 1.51) 1.51 0

lpt1 = Vector3 (-2.5) 7.98 (-8)
lpt2 = Vector3 (-2.5) 7.98 (-12)
lpt3 = Vector3 2.5 7.98 (-12)
lpt4 = Vector3 2.5 7.98 (-8)

mplight = mapUni plight
mceil  = mapUni ceil
mfloor = mapUni floor1
mrwall = mapUni floor2
mbwall = mapUni floor3
mmirr = mapUni ball1
mtrans = mapUni ball2

-- Tetrapot --
tp1 = Vector3 (-1) 0 (-1)
tp2 = Vector3 1 0 (-1)
tp3 = Vector3 1 0 1
tp4 = Vector3 (-1) 0 1
tp5 = Vector3 0 1.4142 0
tp6 = Vector3 0 (-1.4142) 0


