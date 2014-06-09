{-# LANGUAGE NoImplicitPrelude #-}

--
-- Algebra:
--

module Aya.Algebra
  ( Scalable
  , (<*>)
  , (</>)
  , (<.>)
  , (<**>)
  , Vector2(..)
  , ex2
  , ey2
  , Vector3(..)
  , zero
  , norm
  , square
  , normal
  , Point3
  , Direction3
  , ex3
  , ey3
  , ez3
  ) where

import Data.Maybe
import NumericPrelude
import qualified Algebra.Additive as Additive

-----------
-- CLASS --
-----------

class Scalable a where
  (<*>) :: a -> Double -> a
  (</>) :: a -> Double -> Maybe a
  a </> s
    | s == 0    = Nothing
    | otherwise = Just (a <*> (1 / s))

class (Show a, Eq a, Additive.C a, Scalable a) => Matrix a where
  norm   :: a -> Double

class (Matrix a) => Vector a where
  (<.>)  :: a -> a -> Double    -- dot product
  normal :: a -> Maybe a
  normal a = a </> (norm a)
  square :: a -> Double
  square a = a <.> a

----------
-- DATA --
----------

-- 2 dimensional vector
-------------------------------------------------------

data Vector2 = Vector2 Double Double

instance Show Vector2 where
  show (Vector2 ax ay) = "[" ++ (show ax) ++ "," ++ (show ay) ++ "]"

instance Eq Vector2 where
  (==) (Vector2 ax ay) (Vector2 bx by) = (ax == bx) && (ay == by)

instance Additive.C Vector2 where
  zero = Vector2 0 0
  (+) (Vector2 ax ay) (Vector2 bx by) = Vector2 (ax + bx) (ay + by)
  (-) (Vector2 ax ay) (Vector2 bx by) = Vector2 (ax - bx) (ay - by)
  negate (Vector2 ax ay) = Vector2 (-ax) (-ay)

instance Scalable Vector2 where
  (<+>) (Vector2 ax ay) s = Vector2 (ax * s) (ay * s)

instance Matrix Vector2 where
  norm a = sqrt (square a)

instance Vector Vector2 where
  (<.>) (Vector2 ax ay) (Vector2 bx by) = ax * bx + ay * by

ex2 = Vector2 1 0 :: Vector2
ey2 = Vector2 0 1 :: Vector2

-- 3 dimensional vector
-------------------------------------------------------

data Vector3 = Vector3 Double Double Double

instance Show Vector3 where
  show (Vector3 ax ay az) =
      "[" ++ (show ax) ++ "," ++ (show ay) ++ "," ++ (show az) ++ "]"

instance Eq Vector3 where
  (Vector3 ax ay az) == (Vector3 bx by bz) =
      (ax == bx) && (ay == by) && (az == bz)

instance Additive.C Vector3 where
  zero = Vector3 0 0 0
  (+) (Vector3 ax ay az) (Vector3 bx by bz) =
      Vector3 (ax + bx) (ay + by) (az + bz)
  (-) (Vector3 ax ay az) (Vector3 bx by bz) =
      Vector3 (ax - bx) (ay - by) (az - bz)
  negate (Vector3 ax ay az) = Vector3 (-ax) (-ay) (-az)

instance Scalable Vector3 where
  (<*> (Vector3 ax ay az) s = Vector3 (ax * s) (ay * s) (az * s)

instance Matrix Vector3 where
  norm a = sqrt (square a)

instance Vector Vector3 where
  (<.>) (Vector3 ax ay az) (Vector3 bx by bz) = ax * bx + ay * by + az * bz

(<**>) :: Vector3 -> Vector3 -> Vector3  -- cross product
(<**>) (Vector3 ax ay az) (Vector3 bx by bz) =
    Vector3 (ay * bz - by * az) (az * bx - ax * bz) (ax * by - ay * bx)

-- 3 dimensional point
-------------------------------

type Point3 = Vector3

initPoint :: Double -> Double -> Double -> Point3
initPoint x y z = Vector3 x y z

-- 3 dimensional direction vector
-------------------------------------

type Direction3 = Vector3

initDirection :: Double -> Double -> Double -> Point3
initDirection x y z = Vector3 x y z

checkDirection :: Maybe Vector3 -> Maybe Direction3
checkDirection vec
  | vec == Nothing = Nothing
  | otherwise      = normal $ fromJust vec

ex3 = initDirection 1 0 0 :: Direction3
ey3 = initDirection 0 1 0 :: Direction3
ez3 = initDirection 0 0 1 :: Direction3
