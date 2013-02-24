--
--
--

module Scene where

lights = [
  PointLight (Vector3 0 2 0) (Intensity 120 120 120),
  ParallelLight (Vector3 1 (-1) 1) (Intensity 40 40 40)
  ]

primitives = [
  Primitive (Plain (Vector3 0 1 0) 0) (Material),
  Primitive (Sphere (Vector3 0 0.5 0) 0.1) (Material)
  ]

