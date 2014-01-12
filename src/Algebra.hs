--
--
--

module Algebra
  ( (^+)
  , (^-)
  , (^*)
  , (^/)
  , neg
  , norm
  , (^.)
  , normal
  , square
  , Vector2(..)
  , o2
  , ex2
  , ey2
  , Vector3(..)
  , (^**)
  , Point3
  , initPoint
  , o3
  , Direction3
  , initDirection
  , ex3
  , ey3
  , ez3
  ) where

import Data.Maybe

-----------
-- CLASS --
-----------

class (Show a, Eq a) => Matrix a where
  (^+) :: a -> a -> a
  (^-) :: a -> a -> a
  (^*) :: a -> Double -> a
  (^/) :: a -> Double -> Maybe a
  neg :: a -> a
  norm :: a -> Double

class (Matrix a) => Vector a where
  (^.) :: a -> a -> Double
  normal :: a -> Maybe a
  square :: a -> Double

----------
-- DATA --
----------

-- 2 dimensional vector
----------------------------------------------------------

data Vector2 = Vector2 Double Double

instance Show Vector2 where
  show (Vector2 ax ay) = "[" ++ (show ax) ++ "," ++ (show ay) ++ "]"

instance Eq Vector2 where
  (Vector2 ax ay) == (Vector2 bx by) = (ax == bx) && (ay == by)

instance Matrix Vector2 where
  (Vector2 ax ay) ^+ (Vector2 bx by) = Vector2 (ax + bx) (ay + by)
  (Vector2 ax ay) ^- (Vector2 bx by) = Vector2 (ax - bx) (ay - by)
  (Vector2 ax ay) ^* s = Vector2 (ax * s) (ay * s)
  (Vector2 ax ay) ^/ s
    | s == 0    = Nothing
    | otherwise = Just (Vector2 (ax / s) (ay / s))
  neg (Vector2 ax ay) = Vector2 (-ax) (-ay)
  norm a = sqrt (square a)

instance Vector Vector2 where
  (Vector2 ax ay) ^. (Vector2 bx by) = ax * bx + ay * by
  normal a
    | mag == 0  = Nothing
    | otherwise = a ^/ mag
    where mag = norm a
  square a = a ^. a

o2  = Vector2 0 0
ex2 = Vector2 1 0
ey2 = Vector2 0 1

-- 3 dimensional vector
-----------------------------------------------------------

data Vector3 = Vector3 Double Double Double

instance Show Vector3 where
  show (Vector3 ax ay az) = "[" ++ (show ax) ++ "," ++ (show ay) ++ "," ++ (show az) ++ "]"

instance Eq Vector3 where
  (Vector3 ax ay az) == (Vector3 bx by bz) = (ax == bx) && (ay == by) && (az == bz)

instance Matrix Vector3 where
  (Vector3 ax ay az) ^+ (Vector3 bx by bz) = Vector3 (ax + bx) (ay + by) (az + bz)
  (Vector3 ax ay az) ^- (Vector3 bx by bz) = Vector3 (ax - bx) (ay - by) (az - bz)
  (Vector3 ax ay az) ^* s = Vector3 (ax * s) (ay * s) (az * s)
  (Vector3 ax ay az) ^/ s
    | s == 0    = Nothing
    | otherwise = Just (Vector3 (ax / s) (ay / s) (az / s))
  neg (Vector3 ax ay az) = Vector3 (-ax) (-ay) (-az)
  norm a = sqrt (square a)

instance Vector Vector3 where
  (Vector3 ax ay az) ^. (Vector3 bx by bz) = ax * bx + ay * by + az * bz
  normal a
    | mag == 0  = Nothing
    | otherwise = a ^/ mag
    where mag = norm a
  square a = a ^. a

(^**) :: Vector3 -> Vector3 -> Vector3
(Vector3 ax ay az) ^** (Vector3 bx by bz) = Vector3 (ay * bz - by * az) (az * bx - bz * ax) (ax * by - ay * bx)

-- 3 dimensional point
----------------------

type Point3 = Vector3

initPoint :: Double -> Double -> Double -> Point3
initPoint x y z = Vector3 x y z

o3  = initPoint 0 0 0

-- 3 dimentional direction vector
---------------------------------

type Direction3 = Vector3

initDirection :: Double -> Double -> Double -> Maybe Direction3
initDirection x y z = checkDirection (Just $ Vector3 x y z)

checkDirection :: Maybe Vector3 -> Maybe Direction3
checkDirection vec
  | vec == Nothing = Nothing
  | otherwise      = normal $ fromJust vec

ex3 = fromJust $ initDirection 1 0 0
ey3 = fromJust $ initDirection 0 1 0
ez3 = fromJust $ initDirection 0 0 1

