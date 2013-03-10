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
                    idist :: Double, ishape :: Shape, imate :: Material, inout :: Inout
                  } deriving Show

data Primitive = Primitive Shape Material deriving Show

intersect :: Primitive -> Ray -> [Intersection]
intersect (Primitive shp mate) ray = map genIs (distance shp ray)
  where genIs :: (Double, Inout) -> Intersection
        genIs (t, io) = Intersection t shp mate io



