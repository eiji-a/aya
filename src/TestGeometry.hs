
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
  , "Ray" ~: testRay
  , "Fresnel" ~: testFresnel
  ]

--
-- Plain
--

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
  where p1 = initPlain (Vector3 0 1 0) 1
        p2 = initPlain (Vector3 1 1 1) (-10)
        rt13 = 1.0 / sqrt 3.0
        rp1 = initRay (Vector3 0 0 0) (Vector3 0 0 1)
        rp2 = initRay (Vector3 0 0 0) (Vector3 0 (-1) 1)
        rp3 = initRay (Vector3 0 0 0) (Vector3 0 1 1)

--
-- Sphere
--

testSphere :: [Test]
testSphere =
  [ show s1 ~=? "Just [[0.0,0.0,0.0],1.0]"
  , show s2 ~=? "Just [[1.0,1.0,1.0],10.0]"
  , show s3 ~=? "Nothing"
  , side (fromJust s1) (Vector3 2 0 0) ~=? 1.0
  , side (fromJust s1) (Vector3 0.5 0 0) ~=? -0.5
  , distance (fromJust s1) (fromJust r1) ~=? [(2, fromJust s1, Inside), (4, fromJust s1, Outside)]
  ]
  where s1 = initSphere (Vector3 0 0 0) 1
        s2 = initSphere (Vector3 1 1 1) 10
        s3 = initSphere (Vector3 1 1 1) (-5)
        r1 = initRay (Vector3 0 0 (-3)) ez3

--
-- Ray
--

testRay :: [Test]
testRay =
  [ show r1 ~=? "Just (Ray {rpos = [0.0,0.0,0.0], rdir = [1.0,0.0,0.0]})"
  ]
  where r1 = initRay o3 ex3

testFresnel :: [Test]
testFresnel =
  [ tray1 ~=? Nothing
  , kr1 ~=? 1
  , kt1 ~=? 0
  , tray2 ~=? Nothing
  , kr2 ~=? 1
  , kt2 ~=? 0
  , tray3 ~=? initRay pt1 (Vector3 (2 / 3 * (1 / (sqrt 2))) (2 / 3 * ((-1) / (sqrt 2) + (cos1 - g1))) 0)
  , kr3 ~=? 0.04206927312437237
  , kt3 ~=? 1 - kr3
  , tray4 ~=? Nothing
  , kr4 ~=? 1
  , kt4 ~=? 0
  ]
  where pt1 = o3
        edir = fromJust (normal (Vector3 1 (-1) 0))
        cos1 = -(n `dot` edir)
        n    = ey3
        eta1a = 0
        eta2a = 0
        eta1b = 1
        eta2b = 1.5
        eta1c = 1.5
        eta2c = 1
        g1 = sqrt 7 / 2
        (tray1, kr1, kt1) = fresnel pt1 edir n cos1 eta1a eta2b
        (tray2, kr2, kt2) = fresnel pt1 edir n cos1 eta1b eta2a
        (tray3, kr3, kt3) = fresnel pt1 edir n cos1 eta1b eta2b
        (tray4, kr4, kt4) = fresnel pt1 edir n cos1 eta1c eta2c
