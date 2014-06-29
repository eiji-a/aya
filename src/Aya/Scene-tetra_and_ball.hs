{-# LANGUAGE NoImplicitPrelude #-}

--
-- Scene
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
import NumericPrelude

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

xReso = 256 :: Int
yReso = 256 :: Int

xRegion = (-1.0, 1.0)
yRegion = (-1.0, 1.0)

eyepos = Vector3 4 5 (-3)
etarget = Vector3 0 0 0
upper = Vector3 0 1 0
focus = 2.0


material0 =
    Material intensityZero colorBlack colorBlack colorWhite colorWhite 1 0

iAmb = initIntensity colorWhite 100

-- lights and primitives

lights :: [Light]
lights =
  [ 
    DirectiveLight (Vector3 (-4) 7 (-1)) (initIntensity colorWhite 5000)
                   (negate ey3) lp1
--, ParallelLight (Vector3 1 1 (-1)) (initIntensity colorWhite 20)
  , DirectiveLight (Vector3 2 6 5) (initIntensity colorWhite 3000)
                   (negate ey3) lp2
  ]

lp1 = Primitive (fromJust $ initSphere (Vector3 (-1) 3 (-2)) 0.6) mplight
lp2 = Primitive (fromJust $ initSphere (Vector3 1 3 5) 0.6) mplight

primitives :: [Primitive]
primitives =
  [ lp1, lp2
  , Primitive (fromJust $ initPlain (Vector3 0 1 0) 2) mfloor
  , Primitive (fromJust $ initPlain (Vector3 0 (-1) 0) (50)) msky1
--, Primitive (fromJust $ initSphere (Vector3 (-0.5) (-0.5) 3.2) 1) mball1
  , Primitive (fromJust $ initSphere (Vector3 (-2) 0 0) 1) mball2
  , Primitive (fromJust $ initPolygon tp0 tp3 tp1) mtrans
  , Primitive (fromJust $ initPolygon tp0 tp3 tp2) mtrans
  , Primitive (fromJust $ initPolygon tp2 tp3 tp1) mtrans
  , Primitive (fromJust $ initPolygon tp0 tp2 tp1) mtest
--, Primitive (fromJust $ initPolygon (Vector3 0.5 (-1) 0) (Vector3 0 (-0.5) 0)
--            (Vector3 0 (-1) 0.5)) mtest
--, Primitive (fromJust $ initPolygon (Vector3 0 (-1) 0.5) (Vector3 0 (-0.5) 0)
--            (Vector3 (-0.5) (-1) 0)) mtest
--, Primitive (fromJust $ initPolygon (Vector3 0 1 0) (Vector3 0.5 (-1) 0)
--            (Vector3 (-0.5) (-1) 0)) mtrans
--, Primitive (fromJust $ initPolygon (Vector3 0 1 0.3)
--            (Vector3 (-0.5) (-1) 0.3) (Vector3 0.5 (-1) 0.3)) mtrans
  ]


-----
-- details of primitives
-----

plight = Material (initIntensity colorWhite 2000) colorBlack colorBlack
                  colorBlack colorBlack 0 0

sky1   = Material intensityZero (initColor 0.7 0.9 1) colorBlack colorBlack
                  colorBlack 0 0
floor1 = Material intensityZero (initColor 1 0.6 0) (initColor 0.5 0.5 0.5)
                  colorBlack colorBlack 0 0
floor2 = Material intensityZero (initColor 1.0 1.0 0.6) (initColor 1 1 1)
                  colorBlack colorBlack 0 0
ball1  = Material intensityZero (initColor 1 0.5 0.3) (initColor 0.5 1.0 0.8)
                  colorBlack colorBlack 0 0
ball2  = Material intensityZero colorBlack colorWhite colorWhite
                  (Color 1.51 1.51 1.51) 1.51 0
mirr1  = Material intensityZero colorBlack colorWhite colorBlack colorBlack 0 0
msam1  = Material intensityZero (initColor 0 0.2 0.2) colorWhite colorWhite
                  (Color 1.51 1.51 1.51) 1.51 0

mplight = mapUni plight
msky1   = mapUni sky1
mfloor  = mapCheckXZ floor1 floor2 1
mball1  = mapUni ball1
mball2  = mapUni ball2
mtrans  = mapUni ball2
mtest   = mapUni msam1

-- Tetrapot --
tp0 = Vector3 0 0 0
tp1 = Vector3 1 0 1
tp2 = Vector3 1 1 0
tp3 = Vector3 0 1 1

