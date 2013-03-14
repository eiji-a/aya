--
-- SCENE
--

module Scene where

import Algebra
import Geometry
import Physics
import Object

data Scene = Scene {
    clip    :: Double
  , depth   :: Int
  , atomos  :: Material
  , ambient :: Intensity    
  , lights  :: [Light]
  , prims   :: [Primitive]
  }

scene = Scene material0 iAmb lights primitives iclip traceDepth

-- global parameters

iclip :: Double
iclip = 120

traceDepth = 1

material0 = Material intensityBlack intensityBlack IntensityBlack 0 1.0

iAmb = Intensity 0 0 0

-- lights and primitives

lights :: [Light]
lights = [
  PointLight (Vector3 0 2 0) (Intensity 120 120 120),
  ParallelLight (Vector3 1 (-1) 1) (Intensity 40 40 40)
  ]

primitives :: [Primitive]
primitives = [
  Primitive (Plain (Vector3 0 1 0) 0) (Material),
  Primitive (Sphere (Vector3 0 0.5 0) 0.1) (Material)
  ]

