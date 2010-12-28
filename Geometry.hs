module Geometry
       (Vector3 (..),
        vmap,
        vzip,
        vfold,
        dot,
        mag2,
        mag,
        cross,
        normalize,
        Point3 (..),
        vectorFrom,
        pvAdd,
        Ray3 (..),
        Color (..)) where

data Vector3 = Vector3 {vX :: Float, vY :: Float, vZ :: Float} deriving (Show, Eq)

instance Num Vector3 where
  (+) = vzip (+)
  (-) = vzip (-)
  (*) = vzip (*)
  abs = vmap abs
  signum = vmap signum
  fromInteger x = Vector3 x' x' x' where x' = fromInteger x :: Float

vmap :: (Float -> Float) -> Vector3 -> Vector3
vmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

vzip :: (Float -> Float -> Float) -> Vector3 -> Vector3 -> Vector3
vzip f (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (f x1 x2) (f y1 y2) (f z1 z2)

vfold :: (Float -> Float -> Float) -> Vector3 -> Float
vfold f (Vector3 x y z) = f x (f y z)  

-- Vector ops
dot :: Vector3 -> Vector3 -> Float
dot u v = vfold (+) $ vzip (*) u v

mag2 :: Vector3 -> Float
mag2 v = dot v v 
  
mag :: Vector3 -> Float
mag = sqrt . mag2

cross :: Vector3 -> Vector3 -> Vector3
cross (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
  Vector3 (y1*z2-z1*y2) (z1*x2-x1*z2) (x1*y2-y1*x2)
  
normalize :: Vector3 -> Vector3
normalize v = vmap (/ mag v) v

-- Point stuff
data Point3 = Point3 {pX :: Float, pY :: Float, pZ :: Float} deriving (Show, Eq)

vectorFrom :: Point3 -> Point3 -> Vector3
vectorFrom (Point3 x1 y1 z1) (Point3 x2 y2 z2) = Vector3 (x2-x1) (y2-y1) (z2-z1)

pvAdd :: Point3 -> Vector3 -> Point3
pvAdd (Point3 px py pz) (Vector3 vx vy vz) = Point3 (vx+px) (vy+py) (vz+pz)

-- Ray stuff
data Ray3 = Ray3 {origin :: Point3, dir :: Vector3} deriving (Show, Eq)

-- Colors (TODO: Find a better place for this code)
data Color = Color {red :: Float, green :: Float, blue :: Float} deriving (Show, Eq)
