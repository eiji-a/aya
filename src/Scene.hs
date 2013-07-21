--
-- SCENE
--

module Scene where

import Data.Maybe

import Algebra
import Geometry
import Physics
import Object
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


material0 = Material intensityBlack intensityBlack (Intensity 1 1 1) intensityBlack 0 1.0

iAmb = Intensity 0 0 0

-- lights and primitives

lights :: [Light]
lights =
  [ PointLight (Vector3 (-1) 3 1.5) (Intensity 350 350 200)
  , ParallelLight (Vector3 1 1 (-1)) (Intensity 10 30 25)
  ]


floor1 = Material (Intensity 0.5 1.0 1.0) (Intensity 0.5 0.1 0.1) intensityBlack intensityBlack 0 0
ball1 =  Material (Intensity 1 0.5 0.3) (Intensity 0.3 0.5 0.7) intensityBlack intensityBlack 0 0.0
ball2 =  Material intensityBlack intensityWhite intensityWhite intensityBlack 0 1.333


primitives :: [Primitive]
primitives =
  [ Primitive (Plain (Vector3 0 1 0) 2) floor1
  , Primitive (Sphere (Vector3 0 0.0 3.2) 1) ball1
  , Primitive (Sphere (Vector3 0.5 (-1.3) 2) 0.5) ball2
  ]


-------------------------------------------------------------
-- automatic caluculation

eex = upper `cross` eyedir
eey = eex `cross` eyedir
stepx = (snd xRegion - fst xRegion) / fromIntegral xReso
stepy = (snd yRegion - fst yRegion) / fromIntegral yReso
initPoint = (eyedir `scale` focus) `add`
            (eex `scale` ((fst xRegion) + 0.5 * stepx)) `sub`
            (eey `scale` ((snd yRegion) + 0.5 * stepy))

generateRay :: (Double, Double) -> Maybe Ray
generateRay (y, x) = initRay eyepos
                     (initPoint `add` (eex `scale` (stepx * x))
                                `add` (eey `scale` (stepy * y)))

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
