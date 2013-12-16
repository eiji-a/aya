--
-- SCENE
--

module Scene where

import Data.Maybe

import Algebra
import Geometry
import Physics
import Object
import Mapping
import Filter

{--
data Scene = Scene {
    atomos    :: Material
  , ambient   :: Intensity    
  , light     :: [Light]
  , primitive :: [Primitive]
  , clip      :: Double
  , depth     :: Int
  , xreso     :: Int
  , yreso     :: Int
  }


scene = Scene material0 iAmb lights primitives iclip traceDepth xReso yReso
--}

-- global parameters

smooth = True
iclip = 120 :: Double

traceDepth = 5 :: Int

xReso = 256 :: Int
yReso = 256 :: Int

xRegion = (-1.0, 1.0)
yRegion = (-1.0, 1.0)

eyepos = Vector3 0 0.5 (-1)
eyedir = fromJust (normal (Vector3 0 (-0.5) 2))
upper = Vector3 0 1 0
focus = 1.5


material0 = Material intensityBlack intensityBlack (initIntensity 1 1 1 1) intensityBlack 0 1.0

iAmb = initIntensity 0 0 0 0

-- lights and primitives

lights :: [Light]
lights =
  [ PointLight (Vector3 (-1) 3 1.5) (initIntensity 350 350 350 1)
  , ParallelLight (Vector3 1 1 (-1)) (initIntensity 20 20 20 1)
  ]


floor1 = Material (initIntensity 0.8 0 1 1) (initIntensity 0.5 0.5 0.5 1) intensityBlack intensityBlack 0 0
floor2 = Material (initIntensity 1.0 1.0 1.0 1) (initIntensity 1 1 1 1) intensityBlack intensityBlack 0 0
ball1 =  Material (initIntensity 1 0.5 0.3 1) (initIntensity 0.5 1.0 0.8 1) intensityBlack intensityBlack 0 0.0
ball2 =  Material intensityBlack intensityWhite intensityWhite intensityBlack 0 1.333

mfloor = mapCheckXZ floor1 floor2 1
mball1 = mapUni ball1
mball2 = mapUni ball2

primitives :: [Primitive]
primitives =
  [ Primitive (fromJust $ initPlain (Vector3 0 1 0) 2) mfloor
  , Primitive (fromJust $ initSphere (Vector3 (-0.5) (-0.5) 3.2) 1) mball1
  , Primitive (fromJust $ initSphere (Vector3 0.5 (-0.2) 1) 0.5) mball2
  ]


-------------------------------------------------------------
-- automatic caluculation

eex = upper ^** eyedir
eey = eex ^** eyedir
stepx = (snd xRegion - fst xRegion) / fromIntegral xReso
stepy = (snd yRegion - fst yRegion) / fromIntegral yReso
origin = (eyedir ^* focus) ^+
         (eex ^* ((fst xRegion) + 0.5 * stepx)) ^- (eey ^* ((snd yRegion) + 0.5 * stepy))

generateRay :: (Double, Double) -> Maybe Ray
generateRay (y, x) = initRay eyepos (origin ^+ (eex ^* (stepx * x)) ^+ (eey ^* (stepy * y)))

offsetXy :: (Int, Int) -> (Int, Int) -> Int
offsetXy (y, x) (dy, dx)
  | ofst < 0 = error "Offset is negative 1"
  | otherwise = ofst
  where ofst = (y + dy) * xReso + (x + dx)

offsetXy' :: (Int, Int) -> Int
offsetXy' (y, x)
  | ofst < 0 = error "Offset is negative 2"
  | otherwise = ofst
  where ofst = y * xReso + x
