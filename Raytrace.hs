module Raytrace
       (CameraFrame (..),
        ViewPlane (..),
        Window (..),
        Image,
        Scene (..),
        Light (..),
        cameraFromLookat,
        rayThroughPixel,
        renderWindow) where

import Debug.Trace
import Control.Monad
import Data.Maybe
import Geometry
import Surfaces
import Foreign ( newArray )
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL

data Light = Light {light_pos :: Point3, light_col :: Color} deriving (Show)

data CameraFrame = CameraFrame {cam_pt :: Point3,
                                cam_u :: Vector3, cam_v :: Vector3, cam_w :: Vector3} deriving (Show)

data ViewPlane = ViewPlane {view_dist :: Float, view_width :: Float, view_height :: Float} deriving (Show)

data Window = Window {win_width :: Float, win_height :: Float} deriving (Show)

type Image = GLUT.PixelData Float

data Scene = Scene {scene_surfaces :: [Surface], scene_lights :: [Light]}

-- Returns the ray that starts at the eye point and travels through the given pixel
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
rayTrace :: Ray3 -> HitRange -> Scene -> Int -> Color
rayTrace _ _ _ 0 = Color 0 0 0
rayTrace ray hit_range (Scene surfaces lights) depth = 
  colorForHit (Scene surfaces lights) ray closest_hit depth where
    closest_hit = closestHit ray hit_range surfaces
  
-- Finds the closest surface hit by the given range, or Nothing if no surface was hit
closestHit :: Ray3 -> HitRange -> [Surface] -> Maybe HitRecord
closestHit _ _ [] = Nothing
closestHit ray hit_range (x:xs) = if (isJust hit_rec) then hit_rec else next_rec where 
  next_rec = closestHit ray hit_range xs
  next_range = case next_rec of Just rec -> (fst hit_range, hit_time rec)
                                Nothing -> hit_range
  hit_rec = (hit x) ray next_range
  

-- Calculates the color for the specified hit record, taking into account recursion depth
colorForHit :: Scene -> Ray3 -> Maybe HitRecord -> Int -> Color
colorForHit _ _ Nothing depth = Color 0 0 0
colorForHit (Scene surfaces lights) ray (Just hit_rec) depth = 
  calculateLighting lights ray hit_rec depth

calculateLighting :: [Light] -> Ray3 -> HitRecord -> Int -> Color
calculateLighting lights ray hit_rec depth = foldl doLight (Color 0 0 0) lights where
  shades = [lambertianShading, blinnPhongShading ray]
  doLight acc (Light pos col) = let lightDir = normalize $ vectorFrom (hit_pt hit_rec) pos
                                in foldl (\acc2 s -> acc2 + s lightDir col hit_rec) acc shades

lambertianShading :: Vector3 -> Color -> HitRecord -> Color
lambertianShading l_dir l_col (HitRecord material _ _ norm) = cmap (*scale) (l_col * diff) where
  scale = max 0 (dot l_dir norm)
  diff = diffuse material

blinnPhongShading :: Ray3 -> Vector3 -> Color -> HitRecord -> Color
blinnPhongShading (Ray3 _ rayDir) l_dir l_col (HitRecord material _ _ norm) = 
  cmap (*scale) (l_col * spec)  where
    spec = specular material
    view = normalize $ vmap (*(-1)) rayDir
    halfV = normalize $ view + l_dir
    scale = (max 0 (dot halfV norm)) ** (phong_exp material)

-- Calculates the colors of everything in the pixel grid, to be drawn to the screen
renderWindow :: Window -> CameraFrame -> ViewPlane -> Scene -> IO (GLUT.PixelData Float)
renderWindow (Window w h) cf vp scene = 
  liftM (GLUT.PixelData GLUT.RGB GLUT.Float) $ 
  newArray (foldr appendPixel [] [(i, j) | i <- [0..(h-1)], j <- [0..(w-1)]]) where
               appendPixel (i,j) acc = let ray = rayThroughPixel j i (Window w h) cf vp
                                           (Color r g b) = rayTrace ray (1.0005, 99999999) scene 5
                                           in r:g:b:acc

