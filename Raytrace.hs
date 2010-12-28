module Raytrace
       (CameraFrame (..),
        ViewPlane (..),
        Window (..),
        Image,
        cameraFromLookat,
        rayThroughPixel,
        renderWindow) where

import Control.Monad
import Data.Maybe
import Geometry
import Surfaces
import Foreign ( newArray )
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL

data CameraFrame = CameraFrame {cam_pt :: Point3,
                                cam_u :: Vector3, cam_v :: Vector3, cam_w :: Vector3} deriving (Show)

data ViewPlane = ViewPlane {view_dist :: Float, view_width :: Float, view_height :: Float}

data Window = Window {win_width :: Float, win_height :: Float}

type Image = GLUT.PixelData Float

-- Returns the ray the starts at the eye point and travels through the given pixel
-- in the view plane
rayThroughPixel :: Float -> Float -> Window -> CameraFrame -> ViewPlane -> Ray3
rayThroughPixel x y (Window ww wh) (CameraFrame cam c_u c_v c_w) (ViewPlane vp_dist vp_width vp_height) =
  Ray3 cam (Vector3 dir_x dir_y dir_z) where
    left = -vp_width/2.0
    bottom = -vp_height/2.0
    
    u = left + ((x+0.5) / ww) * vp_width
    v = bottom + ((y+0.5) / wh) * vp_height
    w = -vp_dist
    
    dir_x = u*(vX c_u) + v*(vX c_v) + w*(vX c_w)
    dir_y = u*(vY c_u) + v*(vY c_v) + w*(vY c_w)
    dir_z = u*(vZ c_u) + v*(vZ c_v) + w*(vZ c_w)

-- Allows you to specify the camera frame more easily as the camera point,
-- look-at point, and up direction
cameraFromLookat :: Point3 -> Point3 -> Vector3 -> CameraFrame
cameraFromLookat cam look_at up = CameraFrame cam c_u c_v c_w where
  c_w = normalize $ vectorFrom look_at cam
  c_u = normalize $ cross up c_w
  c_v = cross c_w c_u

-- Calculates the color for the given ray in the given time range and recursion depth
rayTrace :: Ray3 -> HitRange -> [Surface] -> Int -> Color
rayTrace _ _ _ 0 = Color 0 0 0
rayTrace ray hit_range surfaces depth = colorForHit closest_hit depth where
  closest_hit = closestHit ray hit_range surfaces

closestHit :: Ray3 -> HitRange -> [Surface] -> Maybe HitRecord
closestHit _ _ [] = Nothing
closestHit ray hit_range (x:xs) = if (isJust hit_rec) then hit_rec else next_rec where 
  next_rec = closestHit ray hit_range xs
  next_range = case next_rec of Just rec -> (fst hit_range, hit_time rec)
                                Nothing -> hit_range
  hit_rec = (hit x) ray next_range
  

-- Calculates the color for the specified hit record, taking into account recursion depth
colorForHit :: Maybe HitRecord -> Int -> Color
colorForHit Nothing depth = Color 0 0 0
colorForHit (Just hit_rec) depth = diffuse where
  (Material diffuse specular reflective phong_exp refr atten) = hit_material hit_rec
  
-- Calculates the colors of everything in the pixel grid, to be drawn to the screen
renderWindow :: Window -> CameraFrame -> ViewPlane -> [Surface] -> IO (GLUT.PixelData Float)
renderWindow (Window w h) cf vp surfaces = 
  liftM (GLUT.PixelData GLUT.RGB GLUT.Float) $ 
  newArray (foldl appendPixel [] [(i, j) | i <- [0..(w-1)], j <- [0..(h-1)]]) where
               appendPixel acc (i,j) = let ray = rayThroughPixel i j (Window w h) cf vp
                                           (Color r g b) = rayTrace ray (1.0005, 99999999) surfaces 5
                                           in r:g:b:acc

