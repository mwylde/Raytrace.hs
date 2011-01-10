module GUI (run) where

import Geometry
import Surfaces
import Surfaces.Sphere
import Surfaces.Plane
import Surfaces.Triangle
import Raytrace
import Graphics.Rendering.OpenGL hiding (Color, Vector3, Light)
import Graphics.UI.GLUT hiding (Color, Vector3, Light, Cube)
import System.CPUTime
import Text.Printf

time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = (fromInteger (end-start)) / (10^12)
  printf "Rendering took %0.3f seconds\n" (diff :: Double)
  return v

run (Scene surfaces lights) cameraFrame viewPlane = do
  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size 800 600
  createWindow "Raytrace.hs"
  
  putStrLn "Started rendering"
  pixbuf <- time $ renderWindow (Window 800 600) cameraFrame viewPlane (Scene surfaces lights)
  
  displayCallback $= display pixbuf
  idleCallback $= Nothing
  mainLoop

display :: PixelData Float -> DisplayCallback
display pixbuf = do
  clear [ ColorBuffer ]
  drawPixels (Size 800 600) pixbuf
  flush
