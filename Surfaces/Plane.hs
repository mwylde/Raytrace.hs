module Surfaces.Plane ( makePlane
                      ) where

import Geometry
import Surfaces

makePlane :: Point3 -> Point3 -> Point3 -> Material -> Surface
makePlane a b c material = Surface hit bbox  Nothing Nothing where
  bbox = BBox 0 0 0 0 0 0
  hit (Ray3 base dir) (t0, t1) =
    if hit_t < t0 || hit_t > t1 then Nothing
    else
      let hit_p = pvAdd base (vmap (*hit_t) dir)
      in Just $ HitRecord material hit_t hit_p n
    where 
      n = normalize $ (vectorFrom a b) `cross` (vectorFrom a c)
      hit_t = (dot (vectorFrom base a) n)/(dot dir n)
