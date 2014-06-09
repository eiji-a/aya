--
-- Main: mainroutine of Aya
--

module Main (
  main
) where

import Aya.Renderer

main = do
  writeFile "aya.ppm" (header ++ imageToStr (smoothen image))

