

module Main(
  main
) where

import Data.Char
import Data.Maybe
import Data.List
import Data.Array
import qualified Data.Map as Map

import Algebra
import Geometry
import Physics
import Raytrace
import Filter
import Scene
import InitWorld

main = do
  writeFile "aya.ppm" (header ++ imageToStr (smoothen image))

header :: String
header = "P3\r\n# aya output\r\n" ++
         (show xReso) ++ " " ++ (show yReso) ++ "\r\n" ++
         "255\r\n"

scrmap :: [(Int, Int)]
scrmap = [(y, x) | y <- [0..(yReso - 1)], x <- [0..(xReso - 1)]]

image :: [(Int, Intensity)]
image = map tracePoint' scrmap

tracer :: Tracer
tracer = Tracer lights primitives

smoothen :: [(Int, Intensity)] -> [Rgb]
smoothen it
  | smooth == False = map clipToRgb it
  | otherwise       = map (retrace imgmap) [0..(yReso * xReso - 1)]
  where
    imgmap = Map.fromList it

clipToRgb :: (Int, Intensity) -> Rgb
clipToRgb it = toRgb iclip (snd it)

retrace :: Map.Map Int Intensity -> Int -> Rgb
retrace imgmap pt
  | need == Nothing = toRgb iclip it
  | otherwise       = toRgb iclip (detailPoint it pt)
  where
    need = find (compareArround imgmap pt) (arrounds xReso)
    it   = fromJust (Map.lookup pt imgmap)

compareArround :: Map.Map Int Intensity -> Int -> Int -> Bool
compareArround imgmap pt ofst
  | it' == Nothing = False
  | otherwise      = fromJust it `idiff` fromJust it' > iclip * 0.05
  where
    pt' = pt + ofst
    it  = Map.lookup pt imgmap
    it' = Map.lookup pt' imgmap

detailPoint :: Intensity -> Int -> Intensity
detailPoint it pt
  | otherwise  = (fst wtotal) !* (1 / snd wtotal)
  where
    iwaight = gaussian ! 12
    wtotal  = foldl waightave (it !* iwaight, iwaight) (map (subtrace pt) [6, 8, 16, 18])
--  wtotal  = foldl waightave (it !* iwaight, iwaight) (map (subtrace pt) [1, 9, 15, 23])

waightave :: (Intensity, Double) -> (Intensity, Double) -> (Intensity, Double)
waightave (a, aw) (b, bw) = (a !+ (b !* bw), aw + bw)

subtrace :: Int -> Int -> (Intensity, Double)
subtrace pt st = (tracePoint (py + sy, px + sx), gus)
  where
    gus = gaussian ! st
    (sy, sx) = filterstep !! st
    px = fromIntegral (pt `mod` xReso)
    py = ((fromIntegral pt) - px) / (fromIntegral xReso)

tracePoint :: (Double, Double) -> Intensity
tracePoint (y, x) = trace tracer eye material0 traceDepth
  where
    eye = generateRay (y, x)

tracePoint' :: (Int, Int) -> (Int, Intensity)
tracePoint' p@(y, x) = (offsetXy' p,
                       tracePoint (fromIntegral y, fromIntegral x))
