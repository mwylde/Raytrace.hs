import Geometry

module Surfaces
       ( HitRecord (..),
         BBox (..),
         Surface) where

data HitRecord a = HitRecord (Surface a) => {surface :: a, hit_time :: Float, 
                                           hit_pt :: Point3, hit_normal :: Vector3}

data BBox {bbleft :: Float, bbright :: Float, 
           bbbottom :: Float, bbtop :: Float, 
           bbnear :: Float, bbfar :: Float} deriving (Show, Eq)

class Surface a where
  -- Hit function, which reports whether the surface was hit within the given range by the given ray   
  hit :: a -> Ray3 -> Range -> Maybe HitRecord
  bbox :: a -> BBox
  diffuse :: a -> Color
  specular :: a -> Color
  reflective :: a -> Color
  phong_exp :: a -> Float
  refr_index :: a -> Float
  atten :: a -> Float
