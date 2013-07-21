--
-- MAPPING
--

module Mapping where

import Algebra
import Physics


-- types

type MapFunc = Vector3 -> Material

--
-- TEXTURE MAPPING
--

-- uni-texture

mapUni :: Material -> Vector3 -> Material
mapUni mate1 pt = mate1

-- check

mapCheckXZ :: Material -> Material -> Double -> Vector3 -> Material
mapCheckXZ mate1 mate2 sc (Vector3 px py pz)
  | even (xmod + zmod) = mate1
  | otherwise          = mate2
  where xmod = floor (px / sc)
        zmod = floor (pz / sc)


