import Geometry
import Surfaces
import Surfaces.Sphere
import Surfaces.Plane
import Raytrace
import Graphics.Rendering.OpenGL hiding (Color, Vector3, Light)
import Graphics.UI.GLUT hiding (Color, Vector3, Light)

red_color = Color 1 0 0
green_color = Color 0 1 0
white_color = Color 1 1 1 
light_grey_color = Color 0.6 0.6 0.6
black_color = Color 0 0 0
purple_color = Color 0.6 0.2 0.5

spheres :: [Surface] 
spheres = foldl doSphere [] [0, (-3) .. (-40)] where
  mat1 = Material green_color white_color black_color 100 (-1) 4
  mat2 = Material purple_color white_color black_color 100 (-1) 3
  mat3 = Material purple_color white_color light_grey_color 100 (-1) 3
  doSphere acc x = let s1 = makeSphere (Point3 x 0 0) 1 mat1
                       s2 = makeSphere (Point3 0 (-x) 0) 0.25 mat2
                       s3 = makeSphere (Point3 0 0 x) 0.25 mat3
    in s1:s2:s3:acc 

plane :: Surface
plane = makePlane (Point3 (-40) (-1) 2) (Point3 2 (-1) 2) (Point3 2 (-1) (-20)) plane_material where
  plane_material = Material light_grey_color white_color black_color 10 (-1) 4
  
surfaces :: [Surface]
surfaces = plane:spheres

lights :: [Light]
lights = [(Light (Point3 50 50 (-50)) (Color 0.75 0.75 0.75))]

cameraFrame :: CameraFrame
cameraFrame = cameraFromLookat (Point3 4 4 4) (Point3 (-1) (-1) (-1)) (Vector3 0 1 0)

viewPlane :: ViewPlane
viewPlane = ViewPlane 4 8 6

main = do
  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size 800 600
  createWindow "Raytrace.hs"
  
  pixbuf <- renderWindow (Window 800 600) cameraFrame viewPlane (Scene surfaces lights)
  
  putStrLn "Finished Rendering"
  
  displayCallback $= display pixbuf
  idleCallback $= Nothing
  mainLoop

display :: PixelData Float -> DisplayCallback
display pixbuf = do
  clear [ ColorBuffer ]
  drawPixels (Size 800 600) pixbuf
  flush
