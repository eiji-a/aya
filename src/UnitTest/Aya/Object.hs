
module TestObject where

import Test.HUnit
import Algebra
import Physics
import Object

main = do
  runTestTT $ test suite

suite:: [Test]
suite =
  [ "Light" ~: testLight
  , "Primitive" ~: testPrimitive
  ]

pl1 = PointLight (Vector3 0 3 0) (Intensity 120 120 80)
pl2 = ParallelLight (Vector3 1 1 (-1)) (Intensity 40 20 40)
testLight :: [Test]
testLight =
  [ lint pl1 ~=? Intensity 120 120 80
  , lint pl2 ~=? Intensity 40 20 40
  , ldir pl1 (Vector3 0 (-1) 0) ~=? (Just (Vector3 0 1 0), 16)
  , ldir pl2 (Vector3 0 (-1) 0) ~=? (Just (Vector3 1 1 (-1)), 1)
  ]

testPrimitive :: [Test]
testPrimitive =
  [
  ]


