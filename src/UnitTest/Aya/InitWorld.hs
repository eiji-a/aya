--
-- InitWorld: tests of InitWorld module
--

module UnitTest.Aya.InitWorld where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import Test.HUnit
import Test.QuickCheck

import Aya.Algebra
import Aya.Geometry
-- import Aya.Scene
import Aya.InitWorld

import UnitTest.Aya.Scene

main :: IO ()
main = defaultMain testSuite

testSuite = hUnitTestToTests $ tests

tests = "Aya InitWorld" ~: [
    "eye params" ~: testEye
  , "generateRay" ~: testGenerateRay
  ]

testEye = test [
    "eyedir" ~: eyedir ~=? ez3
  , "eex" ~: eex ~=? ex3
  , "eey" ~: eey ~=? neg ey3
  , "stepx" ~: stepx ~=? 2.0 / 256
  , "stepy" ~: stepy ~=? 2.0 / 256
  , "origin" ~: origin ~=? Vector3 (-1) 1 1
  ]

testGenerateRay = test [
    generateRay (128, 128) ~=? initRay (Vector3 0 0 0) ez3
  , generateRay (0, 0) ~=? initRay (Vector3 0 0 0) (Vector3 (-1) 1 1)
  , generateRay (0, 128) ~=? initRay (Vector3 0 0 0) (Vector3 0 1 1)
  , generateRay (64, 192) ~=? initRay (Vector3 0 0 0) (Vector3 0.5 0.5 1)
  ]


  