--
-- Geometry: tests of Geometry module
--

module UnitTest.Aya.Geometry where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import Test.HUnit
import Test.QuickCheck

import Data.Maybe

import Aya.Algebra
import Aya.Geometry

main :: IO ()
main = defaultMain testSuite

testSuite = hUnitTestToTests $ tests

tests = "Aya Geometry" ~: [
    "Plain" ~: testPlain
  , "Sphere" ~: testSphere
  , "Polygon" ~: testPolygon
  , "Ray" ~: testRay
  ]

--
-- Plain
--

testPlain = test [
    show p1 ~=? "Just [[0.0,1.0,0.0],1.0]"
  , show p2 ~=? "Just [[" ++ (show rt13) ++ "," ++ (show rt13) ++ "," ++ (show rt13) ++ "],-10.0]"
  , distance (fromJust p1) (fromJust rp1) ~=? []
  , distance (fromJust p1) (fromJust rp2) ~=? [(sqrt 2, fromJust p1, Inside)]
  , distance (fromJust p1) (fromJust rp3) ~=? [(-(sqrt 2), fromJust p1, Outside)]
  , getNormal (fromJust p1) (Vector3 0 (-1) 0) Inside ~=? ey3
  , getNormal (fromJust p1) (Vector3 0 (-1) 0) Outside ~=? neg ey3
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

testSphere = test [
    show s1 ~=? "Just [[0.0,0.0,0.0],1.0]"
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
-- Polygon
--

testPolygon = test [
    p1 ~=? Nothing
  , p2 ~=? Nothing
  , p3 ~=? Nothing
  , p4 ~=? Nothing
  , poly_v p5 ~=? Vector3 1 1 1
  , poly_e1 p5 ~=? Vector3 2 0 4
  , poly_e2 p5 ~=? Vector3 3 0 1
  , poly_n p5 ~=? Vector3 0 10 0
  , side p5 (Vector3 0 2 0) ~=? 10
  , side p5 (Vector3 0 0 0) ~=? -10
  , side p5 (Vector3 1000 2 1000) ~=? 10
  , side p5 (Vector3 1000 0 1000) ~=? -10
  , side p5 (Vector3 1000 (-1) 1000) ~=? -20
  , side p5 (Vector3 1000 1 1000) ~=? 0
  , d1 ~=? [(-1, p5, Outside)]
  , d2 ~=? [(-1, p5, Inside)]
  , d3 ~=? [(1, p5, Inside)]
  , d4 ~=? []
  , d5 ~=? [(sqrt 14, p5, Inside)]
  , n1 ~=? Vector3 0 1 0
  , n2 ~=? Vector3 0 1 0
  , poly_v p6 ~=? Vector3 0 0 0
  , poly_e1 p6 ~=? Vector3 0 0 0.5
  , poly_e2 p6 ~=? Vector3 (-0.5) 0 0.5
  , poly_n p6 ~=? Vector3 0 (-0.25) 0
  , getNormal p5 (Vector3 2 1 2) Outside ~=? Vector3 0 (-1) 0
  , getNormal p5 (Vector3 2 1 2) Inside ~=? Vector3 0 1 0
  , getNormal p7 (Vector3 0 0 (-0.3)) Outside ~=? Vector3 0 1 0
  ]
  where p1 = initPolygon (Vector3 0 0 0) (Vector3 1 2 3) (Vector3 3 6 9)
        p2 = initPolygon o3 (Vector3 1 2 3) (Vector3 3 6 9)
        p3 = initPolygon (Vector3 1 2 3) o3 (Vector3 3 6 9)
        p4 = initPolygon (Vector3 1 2 3) (Vector3 3 6 9) o3
        p5 = fromJust $ initPolygon (Vector3 1 1 1) (Vector3 3 1 5) (Vector3 4 1 2)
        d1 = distance p5 (fromJust (initRay (Vector3 2 2 2) ey3))
        d2 = distance p5 (fromJust (initRay (Vector3 2 0 2) (neg ey3)))
        d3 = distance p5 (fromJust (initRay (Vector3 1 2 1) (neg ey3)))
        d4 = distance p5 (fromJust (initRay (Vector3 2 2 1) (neg ey3)))
        d5 = distance p5 (fromJust (initRay (Vector3 4 2 5) (Vector3 (-2) (-1) (-3))))
        n1 = getNormal p5 (Vector3 1 1 1) Inside
        n2 = getNormal p5 (Vector3 2 2 2) Inside
        p6 = fromJust $ initPolygon (Vector3 0 0 0) (Vector3 0 0 0.5) (Vector3 (-0.5) 0 0.5)
        p7 = fromJust $ initPolygon (Vector3 0 0 (-0.5)) (Vector3 0.5 0 0) (Vector3 (-0.5) 0 0)

--
-- Ray
--

testRay = test [
    show r1 ~=? "Just (Ray {rpos = [0.0,0.0,0.0], rdir = [1.0,0.0,0.0]})"
  ]
  where r1 = initRay o3 ex3

