--
-- Object: tests of Object module
--

module UnitTest.Aya.Object where

import Data.Maybe

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import Test.HUnit
import Test.QuickCheck

import Aya.Algebra
import Aya.Color
import Aya.Geometry
import Aya.Object
import Aya.Material
import Aya.Mapping

main :: IO ()
main = defaultMain testSuite

testSuite = hUnitTestToTests $ tests

tests = "Aya Object" ~: [
    "Light" ~: testLight
  , "Primitive" ~: testPrimitive
  ]

testLight = test [
    lint pl1 vc ~=? initIntensity (initColor 1 1 0.67) 120
  , lint pl2 vc ~=? initIntensity (initColor 1 0.5 1) 40
  , ldir pl1 (Vector3 0 (-1) 0) ~=? (Just (Vector3 0 1 0), 16)
  , ldir pl2 (Vector3 0 (-1) 0) ~=? (Just (Vector3 1 1 (-1)), 1)
  ]
  where
    pl1 = PointLight (Vector3 0 3 0) (initIntensity (initColor 1 1 0.67) 120) prm
    pl2 = ParallelLight (Vector3 1 1 (-1)) (initIntensity (initColor 1 0.5 1) 40)
    vc  = Vector3 1 0 0
    flr = Material intensityZero colorBlack colorBlack colorBlack colorBlack 1.0 0
    mp  = mapUni flr
    prm = Primitive (fromJust $ initPlain (Vector3 0 0 0) 0) mp

testPrimitive = test [
    a1 ~=? 1
  ]
  where
    a1 = 1
