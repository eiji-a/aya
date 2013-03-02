--
--
--

module Algebra where

class (Show a, Eq a) => Matrix a where
  add :: a -> a -> a
  sub :: a -> a -> a
  scale :: a -> Double -> a
  divide :: a -> Double -> a
  negate :: a -> a
  norm :: a -> Double

class (Matrix a) => Vector a where
  dot :: a -> a -> Double
  normal :: a -> a
  square :: a -> Double

-- 2 dimensional vector
-- --------------------

data Vector2 = Vector2 Double Double

instance Show Vector2 where
  show (Vector2 ax ay) = "[" ++ (show ax) ++ "," ++ (show ay) ++ "]"

instance Eq Vector2 where
  (Vector2 ax ay) == (Vector2 bx by) = (ax == bx) && (ay == by)

instance Matrix Vector2 where
  add (Vector2 ax ay) (Vector2 bx by) = Vector2 (ax + bx) (ay + by)

  sub (Vector2 ax ay) (Vector2 bx by) = Vector2 (ax - bx) (ay - by)

  scale (Vector2 ax ay) s = Vector2 (ax * s) (ay * s)

  divide (Vector2 ax ay) s = Vector2 (ax / s) (ay / s)

  negate (Vector2 ax ay) = Vector2 (-ax) (-ay)

  norm (Vector2 ax ay) = sqrt (ax * ax + ay * ay)

instance Vector Vector2 where
  dot (Vector2 ax ay) (Vector2 bx by) = ax * bx + ay * by

  normal (Vector2 ax ay) = Vector2 (ax / mag) (ay / mag)
    where mag = norm (Vector2 ax ay)

  square a = a `dot` a

o2  = Vector2 0 0
ex2 = Vector2 1 0
ey2 = Vector2 0 1

-- 3 dimensional vector
-----------------------

data Vector3 = Vector3 Double Double Double

instance Show Vector3 where
  show (Vector3 ax ay az) = "[" ++ (show ax) ++ ","
                                ++ (show ay) ++ ","
                                ++ (show az) ++ "]"

instance Eq Vector3 where
  (Vector3 ax ay az) == (Vector3 bx by bz) = (ax == bx) &&
                                             (ay == by) &&
                                             (az == bz)

instance Matrix Vector3 where
  add (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ax + bx)
                                                      (ay + by)
                                                      (az + bz)

  sub (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ax - bx)
                                                      (ay - by)
                                                      (az - bz)

  scale (Vector3 ax ay az) s = Vector3 (ax * s) (ay * s) (az * s)

  divide (Vector3 ax ay az) s = Vector3 (ax / s) (ay / s) (az / s)

  negate (Vector3 ax ay az) = Vector3 (-ax) (-ay) (-az)

  norm (Vector3 ax ay az) = sqrt (ax * ax + ay * ay + az * az)

instance Vector Vector3 where
  dot (Vector3 ax ay az) (Vector3 bx by bz) = ax * bx + ay * by + az * bz

  normal (Vector3 ax ay az) = Vector3 (ax / mag) (ay / mag) (az / mag)
    where mag = norm (Vector3 ax ay az)

  square a = a `dot` a

cross :: Vector3 -> Vector3 -> Vector3
cross (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ay * bz - by * az)
                                                      (az * bx - bz * ax)
                                                      (ax * by - ay * bx)

o3  = Vector3 0 0 0
ex3 = Vector3 1 0 0
ey3 = Vector3 0 1 0
ez3 = Vector3 0 0 1

