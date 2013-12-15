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

type LightDirection = (Maybe Vector3, Double)
-- return: L vector and decay factor
ldir :: Light -> Vector3 -> LightDirection
ldir (ParallelLight dir _) _ = (Just dir, 1.0)
ldir (PointLight pos _) pt
  | len == 0  = (Nothing, 0)
  | otherwise = (ldir ^/ len, len2)
  where ldir = pos ^- pt
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
  , ismap   :: MapFunc
  , isn'    :: Vector3
  , inout   :: Inout
  }

instance Eq Intersection' where
  (==) is is' = (isdist is) == (isdist is')

instance Ord Intersection' where
  compare is is'
    | isdist is == isdist is' = EQ
    | isdist is <= isdist is' = LT
    | otherwise               = GT

-- primitive
------------

data Primitive = Primitive Shape MapFunc

intersect :: Primitive -> Ray -> [Intersection']
intersect (Primitive shp mapf) ray = map mkIs' (distance' shp ray)
  where mkIs' = mkIs mapf

mkIs :: MapFunc -> (Double, Shape, Inout) -> Intersection'
mkIs mapf (t, shp, io) = Intersection' t shp mapf o3 io

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
        cos1  = -(n ^. edir)
        edir = rdir ray
        rray = initRay pt ((n ^* (2 * cos1)) ^+ edir)
        mate2 = selectMaterial is pt n mate0
        (tray, kr, kt) = fresnel pt edir n cos1 (refidx mate1) (refidx mate2)

selectMaterial :: Intersection' -> Vector3 -> Vector3 -> Material -> Material
selectMaterial is pt n mate0
  | inout is == Inside = (ismap is) pt
  | otherwise          = mate0

