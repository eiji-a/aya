--
-- Color: tests of Color module
--

module UnitTest.Aya.Color where

import Data.Maybe
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import Test.HUnit
import Test.QuickCheck

import Aya.Algebra
import Aya.Geometry
import Aya.Color

main :: IO ()
main = defaultMain testSuite

testSuite = hUnitTestToTests $ tests

tests = "Aya Color" ~: [
    "rgb" ~: testRgb
  ]

testRgb = test [
  imageToStr [rgb1, rgb2] ~=? "10 15 20 110 215 20 "
  ]
  where
    rgb1 = Rgb 10 15 20
    rgb2 = Rgb 110 215 20
