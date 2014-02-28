--
-- Object: tests of Object module
--

module UnitTest.Aya.Object where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import Test.HUnit
import Test.QuickCheck

import Aya.Algebra
import Aya.Physics
import Aya.Object

main :: IO ()
main = defaultMain testSuite

testSuite = hUnitTestToTests $ tests

tests = "Aya Object" ~: [
    "Light" ~: testLight
  , "Primitive" ~: testPrimitive
  ]

testLight = test [
    lint pl1 ~=? initIntensity 1 1 0.67 120
  , lint pl2 ~=? initIntensity 1 0.5 1 40
  , ldir pl1 (Vector3 0 (-1) 0) ~=? (Just (Vector3 0 1 0), 16)
  , ldir pl2 (Vector3 0 (-1) 0) ~=? (Just (Vector3 1 1 (-1)), 1)
  ]
  where
    pl1 = PointLight (Vector3 0 3 0) (initIntensity 1 1 0.67 120)
    pl2 = ParallelLight (Vector3 1 1 (-1)) (initIntensity 1 0.5 1 40)

testPrimitive = test [
    a1 ~=? 1
  ]
  where
    a1 = 1



