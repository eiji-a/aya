--
--
--

module Physics where

import Algebra

-- RGB
------

data Rgb = Rgb Int Int Int deriving Show

rgbMax = 256

-- intensity (lx)
-----------------

data Intensity = Intensity Double Double Double deriving Show

toRgb :: Intensity -> Double -> Rgb
toRgb (Intensity r g b) l = Rgb ir ig ib
  where ir = minInt $ round (r / l)
        ig = minInt $ round (g / l)
        ib = minInt $ round (b / l)

minInt :: Int -> Int
minInt = min rgbMax

iadd :: Intensity -> Intensity -> Intensity
iadd (Intensity ar ag ab) (Intensity br bg bb) = Intensity (ar + br) (ag + bg) (ab + bb)

iscale :: Intensity -> Double -> Intensity
iscale (Intensity r g b) s = Intensity (r * s) (g * s) (b * s)

decay :: Intensity -> Double -> Intensity
decay i d = iscale i (1.0 / d)

intensity_black = Intensity 0 0 0

--
-- Material
--

-- material
-----------

data Material = Material Double deriving Show





