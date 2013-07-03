--
-- PHYSICS
--

module Physics where

import Algebra

-- RGB
------

data Rgb = Rgb Int Int Int deriving Show

rgbMax :: Int
rgbMax = 256 - 1

rgbMaxDouble :: Double
rgbMaxDouble = fromIntegral rgbMax

rgbToChar :: Rgb -> [Char]
rgbToChar (Rgb r g b) = show r ++ " " ++ show g ++ " " ++ show b ++ " "

imageToStr :: [Rgb] -> [Char]
imageToStr []   = []
imageToStr (x:xs) = rgbToChar x ++ imageToStr xs

-- intensity (lx)
-----------------

data Intensity = Intensity Double Double Double deriving Show

instance Eq Intensity where
  (Intensity ar ag ab) == (Intensity br bg bb) = (ar == br) && (ag == bg) && (ab == bb)

initIntensity :: Double -> Double -> Double -> Double -> Intensity
initIntensity r g b i = (Intensity r g b) `iscale` i

-- l is clipping value to RGB
toRgb :: Double -> Intensity -> Rgb
toRgb l (Intensity r g b) = Rgb ir ig ib
  where ir = toColor r l
        ig = toColor g l
        ib = toColor b l

toColor :: Double -> Double -> Int
toColor c l = min rgbMax (round (rgbMaxDouble * c / l))

iadd :: Intensity -> Intensity -> Intensity
iadd (Intensity ar ag ab) (Intensity br bg bb) = Intensity (ar + br) (ag + bg) (ab + bb)

imul :: Intensity -> Intensity -> Intensity
imul (Intensity ar ag ab) (Intensity br bg bb) = Intensity (ar * br) (ag * bg) (ab * bb)

iscale :: Intensity -> Double -> Intensity
iscale (Intensity r g b) s
  | s <= 0    = intensityBlack
  | otherwise = Intensity (r * s) (g * s) (b * s)

decay :: Intensity -> Double -> Intensity
decay i d = iscale i (1.0 / d)

idiff :: Intensity -> Intensity -> Double
idiff (Intensity ar ag ab) (Intensity br bg bb) = (abs (ar - br)) + (abs (ag - bg)) + (abs (ab - bb))

intensityBlack = Intensity 0 0 0

--
-- Material
--

-- material
-----------

data Material = Material {
    mdiff :: Intensity
  , mspec :: Intensity
  , mtran :: Intensity
  , mamb :: Intensity
  , mgls :: Int
  , refidx :: Double         -- refractive index
  } deriving Show


