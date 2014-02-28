

module TestPhysics where

import Data.Maybe
import Test.HUnit
import Algebra
import Geometry
import Physics

main = do
  runTestTT $ test suite
  return ()

suite :: [Test]
suite =
  [ "fresnel" ~: testFresnel
  ]

--
-- fresnel formula
--

testFresnel :: [Test]
testFresnel =
  [ tray1 ~=? Nothing
  , kr1 ~=? 1
  , tray2 ~=? Nothing
  , kr2 ~=? 1
  , tray3 ~=? initRay pt1 (Vector3 (2 / 3 * (1 / (sqrt 2))) (2 / 3 * ((-1) / (sqrt 2) + (cos1 - g1))) 0)
  , abs (kr3 - 0.05024) < 0.00001 ~=? True
  , tray4 ~=? Nothing
  , kr4 ~=? 1
  , tray90 ~=? initRay ex3 (Vector3 0 (-1) 0)
  , abs (kt90 - 0.96) < 0.000000001 ~=? True
  , abs ((rdir $ fromJust tray00) ^. ey3 - (-0.745356)) < 0.00000001 ~=? True
  , kt00 ~=? 0
  , tray'90 ~=? initRay ex3 (Vector3 0 (-1) 0)
  , abs (kt'90 - 0.96) < 0.000000001 ~=? True
  ]
  where pt1 = o3
        edir = fromJust (normal (Vector3 1 (-1) 0))
        cos1 = -(ey3 ^. edir)
        g1 = sqrt 7 / 2
        step = pi / 2 / (
        (tray1, kr1, kt1) = fresnel pt1 edir ey3 cos1 0 1.5
        (tray2, kr2, kt2) = fresnel pt1 edir ey3 cos1 1 0
        (tray3, kr3, kt3) = fresnel pt1 edir ey3 cos1 1 1.5
        (tray4, kr4, kt4) = fresnel pt1 edir ey3 cos1 1.5 1
        -- air -> glass (0 - 90)
        edir90 = Vector3 0 (-1) 0
        (tray90, kr90, kt90) = fresnel ex3 edir90 ey3 (-ey3 ^. edir90) 1.0 1.5 -- 90
        edir00 = Vector3 1 0 0
        (tray00, kr00, kt00) = fresnel ex3 edir00 ey3 (-ey3 ^. edir00) 1.0 1.5 -- 90
        -- glass -> air (0 - 90)
        edir'90 = Vector3 0 (-1) 0
        (tray'90, kr'90, kt'90) = fresnel ex3 edir90 ey3 (-ey3 ^. edir90) 1.5 1.0 -- 90

