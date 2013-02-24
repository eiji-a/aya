--
--
--

module Object where

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

ldir :: Light -> Ray -> (Vector3, Double)
ldir (PointLight p i) (Ray pos dir) = (ldir `divide` len, len * len)
  where ldir = p `sub` pos
        len = norm ldir
ldir (ParallelLight d i) (Ray pos dir) = (negate d, 1.0)

lint :: Light -> Intensity
lint (PointLight p i) = i
lint (ParallelLight d i) = i

--
-- Primitive
--

-- intersection
---------------

-- primitive
------------

data Intersection = Intersection {
  idist :: Double, ishape :: Shape, imate :: Material, inout :: Bool
  } deriving Show

data Primitive = Primitive Shape Material deriving Show

intersect :: Primitive -> Ray -> [Intersection]
intersect (Primitive s m) r = map gen_is (distance s r)
  where gen_is :: (Double, Bool) -> Intersection
        gen_is (t, b) = Intersection t s m b



