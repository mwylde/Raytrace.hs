module Surfaces.Sphere ( makeSphere
                       ) where

import Geometry
import Surfaces
import Debug.Trace

makeSphere :: Point3 -> Float -> Material -> Surface
makeSphere (Point3 x y z) r material = Surface hit bbox Nothing Nothing where
  bbox = BBox (x-r) (x+r) (y-r) (y+r) (z-r) (z+r)
  hit (Ray3 e d) (t0, t1) = 
    if discriminant < 0 || hit_t < t0 || hit_t > t1 then Nothing
    else Just $ HitRecord material hit_t hit_p hit_n
    where
      center = (Point3 x y z)
      ctr_to_e = (vectorFrom center e)
      b = dot d ctr_to_e
      discriminant = b*b - (mag2 d) * ((mag2 ctr_to_e) - r * r)
      hit_t = findHitTime discriminant b d
      hit_p = pvAdd e (vmap (*hit_t) d)
      hit_n = normalize $ vectorFrom center hit_p 

findHitTime :: Float -> Float -> Vector3 -> Float
findHitTime discriminant b d = (min (-b - sqrt_disc) (-b + sqrt_disc)) / (mag2 d) where
  sqrt_disc = sqrt discriminant
