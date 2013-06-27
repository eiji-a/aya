

module Main(
  main
) where

import Data.Char

import Physics
-- import Scene
import Raytrace


xreso = 200
yreso = 120

header = "P3\r\n# aya output\r\n" ++ (show xreso) ++ " " ++ (show yreso) ++ "\r\n255\r\n"

image = map tracePoint [(y, x) | y <- [0..(yreso - 1)], x <- [0..(xreso - 1)]]

main = do
  writeFile "aya.ppm" (header ++ imageToStr image)

