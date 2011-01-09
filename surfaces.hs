module Surfaces
       ( HitRecord (..),
         Material (..),
         makeOpaqueMat,
         HitRange,
         full_range,
         BBox (..),
         hitBBox,
         combineBBoxes,
         constructBBT,
         Surface (..)) where

import Data.Maybe
import Data.List
import Geometry
import Debug.Trace
import System.Random

data HitRecord = HitRecord  {hit_material :: Material, hit_time :: Float, 
                             hit_pt :: Point3, hit_normal :: Vector3} deriving (Show, Eq)

data Material = Material { diffuse :: Color, specular :: Color,
                           reflective :: Color, phong_exp :: Float, 
                           refr_index :: Maybe Float, atten :: Color} deriving (Show, Eq)
                                                                               
makeOpaqueMat :: Color -> Color -> Color -> Float -> Material
makeOpaqueMat diff spec refl phong = Material diff spec refl phong Nothing (Color 0 0 0)

data BBox = BBox {bbleft :: Float, bbright :: Float, 
                  bbbottom :: Float, bbtop :: Float, 
                  bbnear :: Float, bbfar :: Float} deriving (Show, Eq)

hitBBox :: Ray3 -> HitRange -> BBox -> Bool
hitBBox (Ray3 base dir) (t0, t1) (BBox left right bottom top near far) = and predicates where
  --intersection time with the given axis (A) cutting plane
  intersectsAt dirA baseA dimS dimL = (tAmin, tAmax) where
    aA = 1/(dirA dir)
    tAmin = ((if aA > 0 then dimS else dimL) - (baseA base)) * aA
    tAmax = ((if aA > 0 then dimL else dimS) - (baseA base)) * aA
   
  (mins, maxs) = unzip [intersectsAt vX pX left right, intersectsAt vY pY bottom top,
                        intersectsAt vZ pZ near far]
  
  predicates = [mi <= ma | mi <- mins, ma <- maxs]

combineBBoxes :: BBox -> BBox -> BBox
combineBBoxes (BBox al ar ab at an af) (BBox bl br bb bt bn bf)  = BBox l r b t n f where
  l = min al bl
  r = max ar br
  b = min ab bb
  t = max at bt
  n = min an bn
  f = max af bf
  
makeBBoxNode :: Maybe Surface -> Maybe Surface -> Surface
makeBBoxNode left right = Surface this_hit this_bbox left right where
  makeBBox Nothing Nothing = BBox 0 0 0 0 0 0
  makeBBox Nothing (Just r) = (bbox r)
  makeBBox (Just l) Nothing = (bbox l)
  makeBBox (Just l) (Just r) = combineBBoxes (bbox l) (bbox r)
  
  this_bbox = makeBBox left right
  this_hit ray hit_range =
    if hitBBox ray hit_range this_bbox then let
      hits = catMaybes $ catMaybes $ map (fmap (\x -> (hit x) ray hit_range)) [left, right] in
      if null hits then Nothing
      else Just $ minimumBy (\x y -> (hit_time x) `compare` (hit_time y)) hits
    else Nothing


data Axis = XAxis | YAxis | ZAxis deriving (Enum, Eq, Show)

constructBBT :: [Surface] -> Surface
constructBBT [] = error "No surfaces in list"
constructBBT (x:[]) = makeBBoxNode (Just x) Nothing
constructBBT (x:y:[]) = makeBBoxNode (Just x) (Just y)
constructBBT surfaces = trace ((show $ length lefts) ++ ", " ++ (show $ length rights)) (makeBBoxNode left right) where
  partitionByBestAxis :: [Surface] -> ([Surface], [Surface])
  partitionByBestAxis xs = minimumBy (\x y -> compare (lenDiff x) (lenDiff y)) possibilities where
    possibilities = map (partitionByAxis xs) [XAxis .. ZAxis]
    lenDiff (f,s) = abs $ length f - length s
  
  (lefts, rights) = partitionByBestAxis surfaces

  (lefts', rights') = if (length lefts) > 0 && (length rights) > 0 then (lefts, rights)
                      else splitAt ((length surfaces) `div` 2) surfaces

  left = Just $ constructBBT lefts'
  right = Just $ constructBBT rights'

partitionByAxis :: [Surface] -> Axis -> ([Surface], [Surface])
partitionByAxis surfaces axis = partition (\x -> midpoint x <= midpoints_avg) surfaces where
  (s, l) = case axis of XAxis -> (bbleft, bbright)
                        YAxis -> (bbbottom, bbtop)
                        ZAxis -> (bbnear, bbfar)
  midpoint (Surface _ x _ _) = ((s x) + (l x)) / 2
  midpoints_avg = (foldl (\acc x -> acc + midpoint x) 0 surfaces) / (genericLength surfaces)


type HitRange = (Float, Float)
full_range :: HitRange
full_range = (1.0005, 99999999999)


data Surface = Surface {hit :: Ray3 -> HitRange -> Maybe (HitRecord),
                        bbox :: BBox,
                        leftChild :: Maybe Surface,
                        rightChild :: Maybe Surface}
