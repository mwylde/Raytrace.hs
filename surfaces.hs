module Surfaces
       ( HitRecord (..),
         Material (..),
         HitRange,
         full_range,
         Vertex,
         BBox (..),
         Surface (..)) where

import Geometry

data HitRecord = HitRecord  {hit_material :: Material, hit_time :: Float, 
                             hit_pt :: Point3, hit_normal :: Vector3} deriving (Show, Eq)

data Material = Material { diffuse :: Color, specular :: Color, reflective :: Color,
                           phong_exp :: Float, refr_index :: Float, atten :: Float} deriving (Show, Eq)

data BBox = BBox {bbleft :: Float, bbright :: Float, 
                  bbbottom :: Float, bbtop :: Float, 
                  bbnear :: Float, bbfar :: Float} deriving (Show, Eq)

type HitRange = (Float, Float)
full_range :: HitRange
full_range = (1.0005, 99999999999)

type Vertex = (Point3, Point3, Point3)

data Surface = Surface {hit :: Ray3 -> HitRange -> Maybe (HitRecord),
                        bbox :: BBox,
                        material :: Material,
                        leftChild :: Maybe Surface,
                        rightChild :: Maybe Surface}
