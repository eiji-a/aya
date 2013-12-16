--
-- OBJECT
--

module Object
  ( Light(..)
  , ldir
  , lint
  , Primitive(..)
  , intersect
  , Distance
  , Intersection
  , initIntersection
  , ispt
  , isn
  , isedir
  , iskr
  , iskt
  , isrray
  , istray
  , ismate1
  , ismate2
  ) where

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

data Distance = Distance {
    dtdist  :: Double
  , dtshape :: Shape
  , dtmap   :: MapFunc
  , inout   :: Inout
  }

instance Eq Distance where
  (==) dt dt' = (dtdist dt) == (dtdist dt')

instance Ord Distance where
  compare dt dt'
    | dtdist dt == dtdist dt' = EQ
    | dtdist dt <= dtdist dt' = LT
    | otherwise               = GT

-- primitive
------------

data Primitive = Primitive Shape MapFunc

intersect :: Primitive -> Ray -> [Distance]
intersect (Primitive shp mapf) ray = map mkIs' (frontObjects 0.02 shp ray)
  where mkIs' = mkIs mapf

mkIs :: MapFunc -> PreDistance -> Distance
mkIs mapf (t, shp, io) = Distance t shp mapf io

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

initIntersection :: Maybe Distance -> Ray -> Material -> Material -> Intersection
initIntersection dt ray mate0 mate1 = Intersection mate1 mate2 pt n edir rray tray kr kt
  where dt' = fromJust dt
        pt = target ray (dtdist dt')
        n  = getNormal (dtshape dt') pt (inout dt')
        cos1  = -(n ^. edir)
        edir = rdir ray
        rray = initRay pt ((n ^* (2 * cos1)) ^+ edir)
        mate2 = selectMaterial dt' pt n mate0
        (tray, kr, kt) = fresnel pt edir n cos1 (refidx mate1) (refidx mate2)

selectMaterial :: Distance -> Vector3 -> Vector3 -> Material -> Material
selectMaterial dt pt n mate0
  | inout dt == Inside = (dtmap dt) pt
  | otherwise          = mate0

