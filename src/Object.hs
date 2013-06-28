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

data Light =
    PointLight Vector3 Intensity
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

data Intersection = Intersection {
    isdist :: Double
  , isshape :: Shape
  , ismate :: Material
  , inout :: Inout
  } deriving Show

instance Eq Intersection where
  (==) is is' = (isdist is) == (isdist is')

instance Ord Intersection where
  compare is is'
    | isdist is == isdist is' = EQ
    | isdist is <= isdist is' = LT
    | otherwise               = GT

-- primitive
------------

data Primitive = Primitive Shape Material deriving Show

intersect :: Primitive -> Ray -> [Intersection]
intersect (Primitive shp mate) ray = map mkIs' (distance' shp ray)
  where mkIs' = mkIs mate

mkIs :: Material -> (Double, Shape, Inout) -> Intersection
mkIs mate (t, shp, io) = Intersection t shp mate io

