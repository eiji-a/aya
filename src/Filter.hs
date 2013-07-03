
module Filter where



---
-- filtering

arrounds :: Int -> [Int]
arrounds x = [-1, 1, (-x), x, -(x+1), -(x-1), x-1, x+1]

compxy :: [(Int, Int)]
compxy =
  [(-1,-1), (-1,0), (-1,1),
   ( 0,-1),         ( 0,1),
   ( 1,-1), ( 1,0), ( 1,1)
  ]

gaussian :: [Double]
{--
gaussian =
  [ 2,  7,  12,  7,  2,
    7, 31,  52, 31,  7,
   12, 52, 127, 52, 12,
    7, 31,  52, 31,  7,
    2,  7,  12,  7,  2
  ]
--}
gaussian =
  [ 1,  4,  6,  4, 1,
    4, 16, 24, 16, 4,
    6, 24, 36, 24, 6,
    4, 16, 24, 16, 4,
    1,  4,  6,  4, 1
  ]

filterstep :: [(Double, Double)]
filterstep =
  [(-0.5,-0.5), (-0.3,-0.5), (0.0,-0.5), (0.3,-0.5), (0.5,-0.5),
   (-0.5,-0.3), (-0.3,-0.3), (0.0,-0.3), (0.3,-0.3), (0.5,-0.3),
   (-0.5, 0.0), (-0.3, 0.0), (0.0, 0.0), (0.3, 0.0), (0.5, 0.0),
   (-0.5, 0.3), (-0.3, 0.3), (0.0, 0.3), (0.3, 0.3), (0.5, 0.3),
   (-0.5, 0.5), (-0.3, 0.5), (0.0, 0.5), (0.3, 0.5), (0.5, 0.5)
  ]

