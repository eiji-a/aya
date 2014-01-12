--
-- PHYSICS
--

module Physics
  ( Rgb(..)
  , toRgb
  , imageToStr
  , Intensity
  , initIntensity
  , (!+)
  , (!*)
  , (!**)
  , idiff
  , intensityBlack
  , intensityWhite
  , Material(..)
  , fresnel
  ) where

import Data.Maybe

import Algebra
import Geometry

-- RGB
------

data Rgb = Rgb Int Int Int deriving Show

rgbMax :: Int
rgbMax = 256 - 1

rgbMaxDouble :: Double
rgbMaxDouble = fromIntegral rgbMax

rgbToChar :: Rgb -> [Char]
rgbToChar (Rgb r g b) = show r ++ " " ++ show g ++ " " ++ show b ++ " "

imageToStr :: [Rgb] -> String
imageToStr []     = ""
imageToStr (x:xs) = rgbToChar x ++ imageToStr xs

-- intensity (lx)
-----------------

data Intensity = Intensity Double Double Double deriving Show

instance Eq Intensity where
  (Intensity ar ag ab) == (Intensity br bg bb) = (ar == br) && (ag == bg) && (ab == bb)

initIntensity :: Double -> Double -> Double -> Double -> Intensity
initIntensity r g b i = (Intensity r g b) !* i

-- l is clipping value to RGB
toRgb :: Double -> Intensity -> Rgb
toRgb l (Intensity r g b) = Rgb ir ig ib
  where ir = toColor r l
        ig = toColor g l
        ib = toColor b l

toColor :: Double -> Double -> Int
toColor c l = min rgbMax (round (rgbMaxDouble * c / l))

(!+) :: Intensity -> Intensity -> Intensity
(Intensity ar ag ab) !+ (Intensity br bg bb) = Intensity (ar + br) (ag + bg) (ab + bb)

(!**) :: Intensity -> Intensity -> Intensity
(Intensity ar ag ab) !** (Intensity br bg bb) = Intensity (ar * br) (ag * bg) (ab * bb)

(!*) :: Intensity -> Double -> Intensity
(Intensity r g b) !* s
  | s <= 0    = intensityBlack
  | otherwise = Intensity (r * s) (g * s) (b * s)

decay :: Intensity -> Double -> Intensity
decay i d = i !* (1.0 / d)

idiff :: Intensity -> Intensity -> Double
idiff (Intensity ar ag ab) (Intensity br bg bb) = (abs (ar - br)) + (abs (ag - bg)) + (abs (ab - bb))

intensityBlack = Intensity 0 0 0
intensityWhite = Intensity 1 1 1

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

--
-- fresnel
--
-- pt : point of intersection
-- e  : eye direction
-- n  : normal vector
-- cos1 : inner product of e and n
-- eta1 : refractive index of current object
-- eta2 : refractive index of front object

type Catadioptric = (Maybe Ray, Double, Double)

fresnel :: Point3 -> Direction3 -> Direction3 -> Double -> Double -> Double -> Catadioptric
fresnel pt e n cos1 eta1 eta2
  | eta1 == 0       = (Nothing, 1.0, 0.0)
  | eta2 == 0       = (Nothing, 1.0, 0.0)
  | g2 < 0          = (Nothing, 1.0, 0.0)
  | tdir == Nothing = (Nothing, 1.0, 0.0)
  | otherwise = (tray, kr, kt)
--  | otherwise = (tray, 1.0, 1.0)
  where eta = eta2 / eta1
        g2 = eta * eta + cos1 * cos1 - 1
        g  = sqrt g2
        tdir = (e ^+ (n ^* (cos1 - g))) ^/ eta
        tray = initRay pt $ fromJust tdir
--        n' = (eta - 1) / (eta + 1)
--        r0 = n' * n'
--        kr = r0 + (1 - r0) * ((1 - cos1) ^ 5)
        gp = g + cos1
        gm = g - cos1
        r1 = gm / gp
        r2 = (cos1 * gp - 1) / (cos1 * gm + 1)
        kr = r1 * r1 * (1 + r2 * r2) / 2
        kt = 1 - kr

