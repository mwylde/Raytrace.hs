import Geometry
import Surfaces
import Surfaces.Sphere
import Surfaces.Plane
import Surfaces.Triangle
import Raytrace
import Graphics.Rendering.OpenGL hiding (Color, Vector3, Light)
import Graphics.UI.GLUT hiding (Color, Vector3, Light, Cube)

red_color = Color 1 0 0
green_color = Color 0 1 0
white_color = Color 1 1 1 
light_grey_color = Color 0.6 0.6 0.6
black_color = Color 0 0 0
purple_color = Color 0.5 0.2 0.5
greenish_color = Color 1 0.7 1

-- Left, Right, Bottom, Top, Near, Far
data Cube = Cube Float Float Float Float Float Float

makeCube :: Material -> Cube -> [Surface]
makeCube mat (Cube l r b t n f) = foldl doCube [] triangles where
  doCube acc (a, b, c) = (makeTriangle a b c mat):acc
  lbn = (Point3 l b n) -- left-bottom-near
  lbf = (Point3 l b f) -- left-bottom-far
  ltn = (Point3 l t n) -- etc.
  ltf = (Point3 l t f)
  rbn = (Point3 r b n)
  rbf = (Point3 r b f)
  rtn = (Point3 r t n)
  rtf = (Point3 r t f)
  triangles = [(lbn, lbf, ltn), (lbf, ltn, ltf)]--, -- left
               --(rbn, rbf, rtn), (rbf, rtn, rtf), -- right
               --(lbn, lbf, rbn), (lbf, rbn, rbf), -- bottom
               --(ltn, ltf, rtn), (ltf, rtn, rtf), -- top
               --(lbn, rbn, ltn), (rtn, ltn, rbn), -- near
               --(lbf, ltf, rbf), (ltf, rbf, rtf)] -- far
  
cubes :: [Surface]
cubes = (makeCube mat1 cube2) where
--  cube1 = asd
  cube2 = Cube 0 8 1 8 (-1) 8
  mat1 = Material greenish_color white_color light_grey_color 100 (-1) 3
    
dannersMethod :: [Surface]
dannersMethod = foldl doCube [] indices where
  vertices = [Point3 0 0 1, Point3 8 0 1, Point3 0 8 1, Point3 8 8 1,
              Point3 0 0 (-1), Point3 8 0 (-1), Point3 0 8 (-1), Point3 8 8 (-1)]
  indices = [(0, 1, 3), (0, 3, 2),   --top
             (0, 2, 6), (0, 6, 4),   --left
             (4, 6, 7), (4, 7, 5),   --bottom
             (1, 5, 7), (1, 7, 3),   --right
             (2, 3, 7), (2, 7, 6),   --back
             (0, 4, 5), (0, 5, 1)]   --front
  material = Material red_color light_grey_color black_color 10 (-1) 3
  doCube acc (a,b,c) = (makeTriangle (vertices!!a) (vertices!!b) (vertices!!c) material):acc

spheres :: [Surface] 
spheres = foldl doSphere [] [0, (-3) .. (-40)] where
  mat1 = Material green_color white_color black_color 100 (-1) 4
  mat2 = Material purple_color white_color black_color 100 (-1) 3
  mat3 = Material purple_color white_color light_grey_color 100 (-1) 3
  doSphere acc x = let s1 = makeSphere (Point3 x 0 0) 1 mat1
                       s2 = makeSphere (Point3 0 (-x) 0) 0.25 mat2
                       s3 = makeSphere (Point3 0 0 x) 0.25 mat3
    in s1:s2:s3:acc 
       
purple_spheres :: [Surface]
purple_spheres = [s1, s2, s3] where
  mat = Material purple_color white_color black_color 100 (-1) 3
  mat2 = Material light_grey_color white_color light_grey_color 100 (-1) 3
  s1 = makeSphere (Point3 6 6 1.76) 0.75 mat
  s2 = makeSphere (Point3 5 2 1.76) 0.85 mat
  s3 = makeSphere (Point3 4 1 4) 0.3 mat2

plane :: Surface
plane = makePlane (Point3 0 0 (-1)) (Point3 1 0 (-1)) (Point3 1 1 (-1)) plane_material where
  plane_material = Material light_grey_color white_color black_color 10 (-1) 4
  
surfaces :: [Surface]
surfaces = plane:dannersMethod++purple_spheres where
  mat1 = Material greenish_color white_color light_grey_color 100 (-1) 3
  triangle = makeTriangle (Point3 0 0 1) (Point3 0 0 (-1))  (Point3 0 8 (-1)) mat1

lights :: [Light]
lights = [Light (Point3 50 2 100) (Color 1 1 1),
          Light (Point3 4 12 20) (Color 0.2 0.2 0.2)]

cameraFrame :: CameraFrame
cameraFrame = cameraFromLookat (Point3 4 (-4) 4) (Point3 4 4 1) (Vector3 0 0 1)

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
