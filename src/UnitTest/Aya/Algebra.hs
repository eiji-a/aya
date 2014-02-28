
module TestAlgebra where

import Test.HUnit
import Algebra

main = do
  runTestTT $ test suite
  return ()

suite :: [Test]
suite =
  [ "Vector2" ~: testVector2
  , "Vector3" ~: testVector3
  ]

v1 = Vector2 1 0
v2 = Vector2 2 4
v3 = Vector2 3 5
v4 = Vector2 0 0
testVector2 :: [Test]
testVector2 =
  [ v1 ~=? ex2
  , (show v1) ~=? "[1.0,0.0]"
  , v1 `add` v2 ~=? Vector2 3 4
  , v1 `sub` v2 ~=? Vector2 (-1) (-4)
  , v2 `scale` 1.2 ~=? Vector2 2.4 4.8
  , v2 `divide` 2 ~=? Just (Vector2 1 2)
  , v2 `divide` 0 ~=? Nothing
  , neg v2 ~=? Vector2 (-2) (-4)
  , norm v1 ~=? 1
  , norm v2 ~=? sqrt 20
  , v2 `dot` v3 ~=? 26
  , normal v4 ~=? Nothing
  , normal v3 ~=? Just (Vector2 (3 / sqrt 34) (5 / sqrt 34))
  , square v3 ~=? 34
  ]

w1 = Vector3 1 0 0
w2 = Vector3 2 4 6
w3 = Vector3 3 5 7
w4 = Vector3 0 0 0
testVector3 :: [Test]
testVector3 =
  [ w1 ~=? ex3
  , (show w1) ~=? "[1.0,0.0,0.0]"
  , w1 `add` w2 ~=? Vector3 3 4 6
  , w1 `sub` w2 ~=? Vector3 (-1) (-4) (-6)
  , w2 `scale` 1.2 ~=? Vector3 2.4 4.8 (6 * 1.2)
  , w2 `divide` 0 ~=? Nothing
  , w2 `divide` 2 ~=? Just (Vector3 1 2 3)
  , neg w2 ~=? Vector3 (-2) (-4) (-6)
  , norm w1 ~=? 1
  , norm w2 ~=? sqrt 56
  , w2 `dot` w3 ~=? 68
  , normal w4 ~=? Nothing
  , normal w3 ~=? Just (Vector3 (3 / sqrt 83) (5 / sqrt 83) (7 / sqrt 83))
  , square w3 ~=? 83
  , ex3 `cross` ey3 ~=? ez3
  , w2 `cross` w3 ~=? Vector3 (-2) 4 (-2)
  ]





