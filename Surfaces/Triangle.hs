module Surfaces.Triangle ( makeTriangle
                         ) where

import Geometry
import Surfaces

makeTriangle :: Point3 -> Point3 -> Point3 -> Material -> Surface
makeTriangle (Point3 ax ay az) (Point3 bx by bz) (Point3 cx cy cz) material =
  Surface hit bbox material Nothing Nothing where
    bbox = BBox 0 0 0 0 0 0
    hit (Ray3 base dir) (t0, t1) =
      if hit_t < t0 || hit_t > t1 then Nothing
      else
        let beta = (j*(e*i-h*f) + k*(g*f-d*i) + l*(d*h-e*g))/mm in
        if (beta < 0 || beta > 1) then Nothing
        else
          let gamma = (i*(a*k-j*b) + h*(j*c-a*l) + g*(b*l-k*c))/mm in
          if (gamma < 0 || (beta+gamma) > 1) then Nothing
          else 
            let pA = (Point3 ax ay az) 
                a_to_b = vectorFrom pA (Point3 bx by bz)
                a_to_c = vectorFrom pA (Point3 cx cy cz)
                norm = normalize $ a_to_b `cross` a_to_c
                point= (pvAdd pA (vmap (*beta) a_to_b))
                point' = (pvAdd point (vmap (*gamma) a_to_c)) in
            Just $ HitRecord material hit_t point' norm
      where
        -- Using the notation of Shirely & Marschner, Section 4.4.2
        -- basically, using Cramer's rule to solve a system of linear equations
        a = ax-bx
        b = ay-by
        c = az-bz
        d = ax-cx
        e = ay-cy
        f = az-cz
        g = (vX dir)
        h = (vY dir)
        i = (vZ dir)
        j = ax-(pX base)
        k = ay-(pY base)
        l = az-(pZ base)
        
        mm = a*(e*i - h*f) + b*(g*f-d*i) + c*(d*h-e*g)
        hit_t = (f*(a*k-j*b) + e*(j*c-a*l) + d*(b*l-k*c))/(-mm)
