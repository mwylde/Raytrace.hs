import Geometry
import Surfaces
import Surfaces.Sphere
import Surfaces.Plane
import Surfaces.Triangle
import Raytrace
import GUI

red_color = Color 1 0 0
green_color = Color 0 1 0
white_color = Color 1 1 1 
light_grey_color = Color 0.6 0.6 0.6
black_color = Color 0 0 0
purple_color = Color 0.5 0.2 0.5
greenish_color = Color 1 0.7 1

cameraFrame :: CameraFrame 
cameraFrame = cameraFromLookat (Point3 4 (-4) 4) (Point3 4 4 1) (Vector3 0 0 1)

viewPlane :: ViewPlane
viewPlane = ViewPlane 4 8 6

lights = [Light (Point3 50 2 100) (Color 1 1 1),
          Light (Point3 4 12 20) (Color 0.2 0.2 0.2)]
         
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
  material = makeOpaqueMat red_color light_grey_color white_color 1000
  doCube acc (a,b,c) = (makeTriangle (vertices!!a) (vertices!!b) (vertices!!c) material):acc

purple_spheres :: [Surface]
purple_spheres = [s1, s2, s3] where
  mat = makeOpaqueMat purple_color white_color black_color 100
  mat2 = makeOpaqueMat black_color white_color white_color 1000000
  s1 = makeSphere (Point3 6 6 1.76) 0.75 mat
  s2 = makeSphere (Point3 5 2 1.76) 0.85 mat
  s3 = makeSphere (Point3 2 4 2.01) 1 mat2

transSphere :: Surface
transSphere = makeSphere (Point3 4 2 3) 1 trans_mat where
  trans_mat = Material black_color white_color black_color 100 (Just 2) (Color 1 0.7 1)
  
plane :: Surface
plane = makePlane (Point3 0 0 (-1)) (Point3 1 0 (-1)) (Point3 1 1 (-1)) plane_material where
  plane_material = makeOpaqueMat light_grey_color white_color black_color 100

surfaces :: [Surface]
surfaces = [plane, transSphere, (constructBBT (dannersMethod++purple_spheres))]

scene :: Scene
scene = Scene surfaces lights

main = do
  run scene cameraFrame viewPlane
