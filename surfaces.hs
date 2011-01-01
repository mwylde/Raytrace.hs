module Surfaces
       ( HitRecord (..),
         Material (..),
         HitRange,
         full_range,
         BBox (..),
         hit_bbox,
         Surface (..)) where

import Geometry

data HitRecord = HitRecord  {hit_material :: Material, hit_time :: Float, 
                             hit_pt :: Point3, hit_normal :: Vector3} deriving (Show, Eq)

data Material = Material { diffuse :: Color, specular :: Color, reflective :: Color,
                           phong_exp :: Float, refr_index :: Float, atten :: Float} deriving (Show, Eq)

data BBox = BBox {bbleft :: Float, bbright :: Float, 
                  bbbottom :: Float, bbtop :: Float, 
                  bbnear :: Float, bbfar :: Float} deriving (Show, Eq)

hit_bbox :: Ray3 -> HitRange -> BBox -> Bool
hit_bbox (Ray3 base dir) (t0, t1) (BBox left right bottom top near far) = and predicates where
  --intersection time with the given axis (A) cutting plane
  intersectsAt dirA baseA dimS dimL = (tAmin, tAmax) where
    aA = 1/(dirA dir)
    tAmin = ((if aA > 0 then dimS else dimL) - (baseA base)) * aA
    tAmax = ((if aA > 0 then dimL else dimS) - (baseA base)) * aA
   
  (mins, maxs) = unzip [intersectsAt vX pX left right, intersectsAt vY pY bottom top,
                        intersectsAt vZ pZ near far]
  
  predicates = [mi <= ma | mi <- mins, ma <- maxs]

type HitRange = (Float, Float)
full_range :: HitRange
full_range = (1.0005, 99999999999)


data Surface = Surface {hit :: Ray3 -> HitRange -> Maybe (HitRecord),
                        bbox :: BBox,
                        material :: Material,
                        leftChild :: Maybe Surface,
                        rightChild :: Maybe Surface}
