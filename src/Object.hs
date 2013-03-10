--
-- OBJECT
--

module Object where

import Data.Maybe
import Algebra
import Geometry
import Physics

--
-- light
--

data Light = PointLight Vector3 Intensity
           | ParallelLight Vector3 Intensity
           deriving Show

-- return: L vector and decay factor
ldir :: Light -> Vector3 -> (Vector3, Double)
ldir (ParallelLight dir _) _ = (neg dir, 1.0)
ldir (PointLight pos _) pt
  | len == 0  = error "light vector is zero"
  | otherwise = (fromJust (ldir `divide` len), len2)
  where ldir = pos `sub` pt
        len2 = square ldir
        len = sqrt len2

lint :: Light -> Intensity
lint (PointLight _ i) = i
lint (ParallelLight _ i) = i

--
-- Primitive
--

-- intersection
---------------

-- primitive
------------

data Intersection = Intersection {
<<<<<<< HEAD
                    idist :: Double, ishape :: Shape, imate :: Material, inout :: Inout
                  } deriving Show
=======
  isdist :: Double, isshape :: Shape, ismate :: Material, inout :: Inout
  } deriving Show
>>>>>>> update 2013/3/9 mac

instance Ord Intersection where
  compare is is'
    | isdist is == isdist is' = EQ
    | isdist is <= isdist is' = LT
    | otherwise               = GT

<<<<<<< HEAD
intersect :: Primitive -> Ray -> [Intersection]
intersect (Primitive shp mate) ray = map genIs (distance shp ray)
  where genIs :: (Double, Inout) -> Intersection
        genIs (t, io) = Intersection t shp mate io
=======
data Primitive = Primitive Shape Material
               | CsgModel CsgOpe Primitive Primitive
               deriving Show
>>>>>>> update 2013/3/9 mac

intersect :: Primitive -> Ray -> [Intersection]
intersect (Primitive s m) r = map genIs (distance s r) m

genIs :: (Shape, Double, Inout) -> Material -> Intersection
genIs (s, t, io) m = Intersection t s m io

