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
  PointLight Vector3 Intensity |
  ParallelLight Vector3 Intensity
  deriving Show

-- return: L vector and decay factor
ldir :: Light -> Vector3 -> (Vector3, Double)
ldir (ParallelLight d _) _ = (neg d, 1.0)
ldir (PointLight p _) pos
  | len == 0  = error "light vector is zero"
  | otherwise = (fromJust (ldir `divide` len), len2)
  where ldir = p `sub` pos
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
  idist :: Double, ishape :: Shape, imate :: Material, inout :: Inout
  } deriving Show

data Primitive = Primitive Shape Material deriving Show

intersect :: Primitive -> Ray -> [Intersection]
intersect (Primitive s m) r = map genIs (distance s r)
  where genIs :: (Double, Inout) -> Intersection
        genIs (t, io) = Intersection t s m io



