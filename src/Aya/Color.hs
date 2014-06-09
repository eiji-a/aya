{-# LANGUAGE NoImplicitPrelude #-}

--
-- Color:
--

module Aya.Color
  ( Rgb(..)
  , toRgb
  , imageToStr
  , Color(..)
  , initColor
  , normalColor
  , colorZero
  , colorBlack
  , colorWhite
  , creverse
  , Intensity
  , initIntensity
  , (!**)
  , (!--)
  , intensityZero
  ) where

import Data.Maybe
import NumericPrelude
import qualified Algebra.Additive as Additive

import Aya.Algebra
import Aya.Geometry

-----------
-- CLASS --
-----------

class Mixable a where
  (!**) :: a -> Color -> a

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

-- Color
--------

data Color = Color {
    cred   :: Double
  , cgreen :: Double
  , cblue  :: Double
  } deriving Show

instance Eq Color where
  (==) (Color r1 g1 b1) (Color r2 g2 b2) =
      (r1 == r2) && (g1 == g2) && (b1 == b2)

instance Scalable Color where
  (<*>) (Color r g b) s = Color (r * s) (g * s) (b * s)

instance Mixable Color where
  (!**) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 * r2) (g1 * g2) (b1 * b2)

initColor :: Double -> Double -> Double -> Color
initColor r g b = Color (round' r) (round' g) (round' b)
  where
    round' :: Double -> Double
    round' a = if a > 1 then 1 else (if a < 0 then 0 else a)

normalColor :: Color -> Color
normalColor (Color r g b) = Color (r * mag) (g * mag) (b * mag)
  where
    mag' = max' r (max' g b)
    mag  = if mag' == 0 then 0 else 1.0 / mag
    max' :: Double -> Double -> Double
    max' a b = if a > b then a else b

creverse :: Color -> Color
creverse (Color r g b) = Color (1 - r) (1 - g) (1 - b)

colorBlack = Color 0 0 0
colorWhite = creverse colorBlack
colorZero  = colorBlack

-- intensity (lx)
-----------------

data Intensity = Intensity Double Double Double deriving Show

instance Eq Intensity where
  (==) (Intensity r1 g1 b1) (Intensity r2 g2 b2) =
      (r1 == r2) && (g1 == g2) && (b1 == b2)

instance Additive.C Intensity where
  zero = initIntensity colorBlack 0
  (+) (Intensity r1 g1 b1) (Intensity r2 g2 b2) =
      Intensity (r1 + r2) (g1 + g2) (b1 + b2)
  negate (Intensity r g b) = Intensity (-r) (-g) (-b)

instance Scalable Intensity where
  (Intensity r g b) <*> s
    | s <= 0    = initIntensity colorBlack 1
    | otherwise = Intensity (r * s) (g * s) (b * s)
  (</>) i d
    | d /= 0    = Just (i <*> (1.0 / d))
    | otherwise = Just (i)

instance Mixable Intensity where
  (!**) (Intensity r g b) (Color cr cg cb) =
      Intensity (r * cr) (g * cg) (b * cb)

initIntensity :: Color -> Double -> Intensity
initIntensity (Color r g b) i = (Intensity r g b) <*> i

-- l is clipping value to RGB
toRgb :: Double -> Intensity -> Rgb
toRgb l (Intensity r g b) = Rgb (toInt r l) (toInt g l) (toInt b l)
  where
    toInt :: Double -> Double -> Int
    toInt c l = if rgbMax < rgb then rgbMax else rgb
      where
        rgb = round (rgbMaxDouble * c / l)

(!--) :: Intensity -> Intensity -> Double
(Intensity r1 g1 b1) !-- (Intensity r2 g2 b2) =
  (abs' r1 r2) + (abs' g1 g2) + (abs' b1 b2)
  where
    abs' :: Double -> Double -> Double
    abs' a b = if a > b then a - b else b - a

intensityZero = Intensity 0 0 0 :: Intensity

