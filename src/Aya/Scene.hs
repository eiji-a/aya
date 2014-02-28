--
-- SCENE
--

module Scene
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

import Algebra
import Geometry
import Physics
import Object
import Mapping

-- global parameters

smooth = True
iclip = 120 :: Double

traceDepth = 6 :: Int

xReso = 256 :: Int
yReso = 256 :: Int

xRegion = (-1.0, 1.0)
yRegion = (-1.0, 1.0)

eyepos = Vector3 2 2 (-1.5)
etarget = Vector3 0 0 0
upper = Vector3 0 1 0
focus = 1.2


material0 = Material intensityBlack intensityBlack (initIntensity 1 1 1 1) intensityBlack 0 1.0

iAmb = initIntensity 1 1 1 100

-- lights and primitives

lights :: [Light]
lights =
  [ 
    PointLight (Vector3 (-1) 3 (-2)) (initIntensity 1 1 1 1500)
--  , ParallelLight (Vector3 1 1 (-1)) (initIntensity 20 20 20 1)
  , PointLight (Vector3 1 3 5) (initIntensity 1 1 1 1500)
  ]

primitives :: [Primitive]
primitives =
  [ Primitive (fromJust $ initPlain (Vector3 0 1 0) 2) mfloor
  , Primitive (fromJust $ initPlain (Vector3 0 (-1) 0) (50)) msky1
--  , Primitive (fromJust $ initSphere (Vector3 (-0.5) (-0.5) 3.2) 1) mball1
  , Primitive (fromJust $ initSphere (Vector3 (-2) 0 2) 1) mball2
  , Primitive (fromJust $ initPolygon tp0 tp3 tp1) mtrans
  , Primitive (fromJust $ initPolygon tp0 tp3 tp2) mtrans
  , Primitive (fromJust $ initPolygon tp2 tp3 tp1) mtrans
  , Primitive (fromJust $ initPolygon tp0 tp2 tp1) mtest
--  , Primitive (fromJust $ initPolygon (Vector3 0.5 (-1) 0) (Vector3 0 (-0.5) 0) (Vector3 0 (-1) 0.5)) mtest
--  , Primitive (fromJust $ initPolygon (Vector3 0 (-1) 0.5) (Vector3 0 (-0.5) 0) (Vector3 (-0.5) (-1) 0)) mtest
--  , Primitive (fromJust $ initPolygon (Vector3 0 1 0) (Vector3 0.5 (-1) 0) (Vector3 (-0.5) (-1) 0)) mtrans
--  , Primitive (fromJust $ initPolygon (Vector3 0 1 0.3) (Vector3 (-0.5) (-1) 0.3) (Vector3 0.5 (-1) 0.3)) mtrans
  ]

-----
-- details of primitives
-----

sky1 = Material (initIntensity 0.7 0.9 1 1) intensityBlack intensityBlack intensityBlack 0 0
floor1 = Material (initIntensity 1 0.6 0  1) (initIntensity 0.5 0.5 0.5 1) intensityBlack intensityBlack 0 0
floor2 = Material (initIntensity 1.0 1.0 0.6 1) (initIntensity 1 1 1 1) intensityBlack intensityBlack 0 0
ball1 =  Material (initIntensity 1 0.5 0.3 1) (initIntensity 0.5 1.0 0.8 1) intensityBlack intensityBlack 0 0.0
ball2 =  Material intensityBlack intensityWhite intensityWhite intensityBlack 0 1.51
mirr1 =  Material intensityBlack intensityWhite intensityBlack intensityBlack 0 0
msam1 =  Material (initIntensity 0 1 1 0.2) intensityWhite intensityWhite intensityBlack 0 1.51

msky1  = mapUni sky1
mfloor = mapCheckXZ floor1 floor2 1
mball1 = mapUni ball1
mball2 = mapUni ball2
mtrans = mapUni ball2
mtest  = mapUni msam1

-- Tetrapot --
tp0 = Vector3 0 0 0
tp1 = Vector3 1 0 1
tp2 = Vector3 1 1 0
tp3 = Vector3 0 1 1

