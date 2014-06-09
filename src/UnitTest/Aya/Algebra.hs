{-# LANGUAGE NoImplicitPrelude #-}

--
-- Algebra: tests of Algebra module
--

module UnitTest.Aya.Algebra where

import NumericPrelude

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import Test.HUnit
import Test.QuickCheck

import Aya.Algebra

main :: IO ()
main = defaultMain testSuite

testSuite = hUnitTestToTests $ tests

tests = "Aya Algebra" ~: [
    "Vector2" ~: testVector2
  , "Vector3" ~: testVector3
  ]

testVector2 = test [
    v1 ~=? ex2
  , (show v1) ~=? "[1.0,0.0]"
  , v1 + v2 ~=? Vector2 3 4
  , v1 - v2 ~=? Vector2 (-1) (-4)
  , v2 <*> 1.2 ~=? Vector2 2.4 4.8
  , v2 </> 2 ~=? Just (Vector2 1 2)
  , v2 </> 0 ~=? Nothing
  , negate v2 ~=? Vector2 (-2) (-4)
  , norm v1 ~=? 1
  , norm v2 ~=? sqrt 20
  , v2 <.> v3 ~=? 26
  , normal v4 ~=? Nothing
  , normal v3 ~=? Just (Vector2 (3 / sqrt 34) (5 / sqrt 34))
  , square v3 ~=? 34
  ]
  where
    v1 = Vector2 1 0
    v2 = Vector2 2 4
    v3 = Vector2 3 5
    v4 = Vector2 0 0

testVector3 = test [
    w1 ~=? ex3
  , (show w1) ~=? "[1.0,0.0,0.0]"
  , w1 + w2 ~=? Vector3 3 4 6
  , w1 - w2 ~=? Vector3 (-1) (-4) (-6)
  , w2 <*> 1.2 ~=? Vector3 2.4 4.8 (6 * 1.2)
  , w2 </> 0 ~=? Nothing
  , w2 </> 2 ~=? Just (Vector3 1 2 3)
  , negate w2 ~=? Vector3 (-2) (-4) (-6)
  , norm w1 ~=? 1
  , norm w2 ~=? sqrt 56
  , w2 <.> w3 ~=? 68
  , normal w4 ~=? Nothing
  , normal w3 ~=? Just (Vector3 (3 / sqrt 83) (5 / sqrt 83) (7 / sqrt 83))
  , square w3 ~=? 83
  , ex3 <**> ey3 ~=? ez3
  , w2 <**> w3 ~=? Vector3 (-2) 4 (-2)
  ]
  where
    w1 = Vector3 1 0 0
    w2 = Vector3 2 4 6
    w3 = Vector3 3 5 7
    w4 = Vector3 0 0 0





