--
-- OBJECT
--

module Object where

import Data.Maybe
import Algebra
import Geometry
import Physics
import Mapping

--
-- light
--

data Light =
    PointLight Vector3 Intensity
  | ParallelLight Vector3 Intensity
  deriving Show

-- return: L vector and decay factor
ldir :: Light -> Vector3 -> (Maybe Vector3, Double)
ldir (ParallelLight dir _) _ = (Just dir, 1.0)
ldir (PointLight pos _) pt
  | len == 0  = (Nothing, 0)
  | otherwise = (ldir `divide` len, len2)
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

data Intersection' = Intersection' {
    isdist  :: Double
  , isshape :: Shape
  , ismate' :: Material
  , isn'    :: Vector3
  , inout   :: Inout
  } deriving Show

instance Eq Intersection' where
  (==) is is' = (isdist is) == (isdist is')

instance Ord Intersection' where
  compare is is'
    | isdist is == isdist is' = EQ
    | isdist is <= isdist is' = LT
    | otherwise               = GT

-- primitive
------------

data Primitive = Primitive Shape Material deriving Show

intersect :: Primitive -> Ray -> [Intersection']
intersect (Primitive shp mate) ray = map mkIs' (distance' shp ray)
  where mkIs' = mkIs mate

mkIs :: Material -> (Double, Shape, Inout) -> Intersection'
mkIs mate (t, shp, io) = Intersection' t shp mate o3 io

data Intersection = Intersection
  { ismate1 :: Material
  , ismate2 :: Material
  , ispt    :: Vector3
  , isn     :: Vector3
  , isedir  :: Vector3
  , isrray  :: Maybe Ray
  , istray  :: Maybe Ray
  , iskr    :: Double
  , iskt    :: Double
  } deriving Show

initIntersection :: Maybe Intersection' -> Ray -> Material -> Material -> Intersection
initIntersection is' ray mate0 mate1 = Intersection mate1 mate2 pt n edir rray tray kr kt
  where is = fromJust is'
        pt = target ray (isdist is)
        n  = getNormal' (isshape is) pt (inout is)
        cos1  = -(n `dot` edir)
        edir = rdir ray
        rray = initRay pt ((n `scale` (2 * cos1)) `add` edir)
        mate2 = if (inout is) == Inside then ismate' is else mate0
        (tray, kr, kt) = fresnel pt edir n cos1 (refidx mate1) (refidx mate2)
