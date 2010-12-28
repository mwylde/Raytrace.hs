import Geometry
import Surfaces
import Surfaces.Sphere
import Raytrace
import Graphics.Rendering.OpenGL hiding (Color, Vector3)
import Graphics.UI.GLUT hiding (Color, Vector3)

red_color = Color 1 0 0
green_color = Color 0 1 0
white_color = Color 1 1 1 
light_grey_color = Color 0.6 0.6 0.6

surfaces :: [Surface]
surfaces = [makeSphere (Point3 6 6 1.76) 0.75 material] where
  material = Material red_color red_color light_grey_color 100 (-1) 4

cameraFrame :: CameraFrame
cameraFrame = cameraFromLookat (Point3 4 (-4) 4) (Point3 4 4 1) (Vector3 0 0 1)

viewPlane :: ViewPlane
viewPlane = ViewPlane 4 8 6

main = do
  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size 800 600
  createWindow "Raytrace.hs"
  
  pixbuf <- renderWindow (Window 800 600) cameraFrame viewPlane surfaces
  
  putStrLn "Finished Rendering"
  
  displayCallback $= display pixbuf
  idleCallback $= Nothing
  mainLoop

display :: PixelData Float -> DisplayCallback
display pixbuf = do
  clear [ ColorBuffer ]
  drawPixels (Size 800 600) pixbuf
  flush
