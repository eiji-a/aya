
module TestGeometry where

import Data.Maybe
import Test.HUnit
import Algebra
import Geometry

main = do
  runTestTT $ test suite
  return ()

suite :: [Test]
suite =
  [ "Plain" ~: testPlain
  , "Sphere" ~: testSphere
  ]

p1 = initPlain (Vector3 0 1 0) 1
p2 = initPlain (Vector3 1 1 1) (-10)
rt13 = 1.0 / sqrt 3.0
rp1 = initRay (Vector3 0 0 0) (Vector3 0 0 1)
rp2 = initRay (Vector3 0 0 0) (Vector3 0 (-1) 1)
rp3 = initRay (Vector3 0 0 0) (Vector3 0 1 1)
testPlain :: [Test]
testPlain =
  [ show p1 ~=? "Just [[0.0,1.0,0.0],1.0]"
  , show p2 ~=? "Just [[" ++ (show rt13) ++ "," ++ (show rt13) ++ "," ++ (show rt13) ++ "],-10.0]"
  , distance (fromJust p1) (fromJust rp1) ~=? []
  , distance (fromJust p1) (fromJust rp2) ~=? [(sqrt 2, fromJust p1, Inside)]
  , distance (fromJust p1) (fromJust rp3) ~=? [(-(sqrt 2), fromJust p1, Outside)]
  , getNormal' (fromJust p1) (Vector3 0 (-1) 0) Inside ~=? ey3
  , getNormal' (fromJust p1) (Vector3 0 (-1) 0) Outside ~=? neg ey3
  ]

s1 = initSphere (Vector3 0 0 0) 1
s2 = initSphere (Vector3 1 1 1) 10
s3 = initSphere (Vector3 1 1 1) (-5)
r1 = initRay (Vector3 0 0 (-3)) ez3
testSphere :: [Test]
testSphere =
  [ show s1 ~=? "Just [[0.0,0.0,0.0],1.0]"
  , show s2 ~=? "Just [[1.0,1.0,1.0],10.0]"
  , show s3 ~=? "Nothing"
  , side (fromJust s1) (Vector3 2 0 0) ~=? 1.0
  , side (fromJust s1) (Vector3 0.5 0 0) ~=? -0.5
  , distance (fromJust s1) (fromJust r1) ~=? [(2, fromJust s1, Inside), (4, fromJust s1, Outside)]
  ]
