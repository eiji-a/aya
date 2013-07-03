
module TestScene where

import Test.HUnit
import Algebra
import Geometry
import Scene

main = do
  runTestTT $ test suite
  return ()

suite :: [Test]
suite =
  [ "eye params" ~: testEye
  , "generateRay" ~: testGenerateRay
  ]

testEye :: [Test]
testEye =
  [ eyedir ~=? ez3
  , eex ~=? ex3
  , eey ~=? neg ey3
  , stepx ~=? 2.0 / 256
  , stepy ~=? 2.0 / 256
  , initPoint ~=? Vector3 (-1) 1 1
  ]

testGenerateRay :: [Test]
testGenerateRay =
  [ generateRay (128, 128) ~=? initRay (Vector3 0 0 0) ez3
  , generateRay (0, 0) ~=? initRay (Vector3 0 0 0) (Vector3 (-1) 1 1)
  , generateRay (0, 128) ~=? initRay (Vector3 0 0 0) (Vector3 0 1 1)
  , generateRay (64, 192) ~=? initRay (Vector3 0 0 0) (Vector3 0.5 0.5 1)
  ]


  