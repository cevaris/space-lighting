module GLUtils where
import Data.IORef ( IORef, newIORef )

import Numeric

import Graphics.Rendering.OpenGL.Raw.ARB.WindowPos
import Graphics.UI.GLUT


data State = State {
   frames  :: IORef Int,
   t0      :: IORef Int,
   ph'     :: IORef Float,
   th'     :: IORef Float,
   gr'     :: IORef Float,
   zh'     :: IORef Float,
   asp     :: IORef Float,
   fov     :: IORef Float,
   dim     :: IORef Float,
   info    :: IORef (String,String)
}

makeState :: IO State
makeState = do
  f  <- newIORef 0
  t  <- newIORef 0
  ph <- newIORef 0
  th <- newIORef 0
  gr <- newIORef 0
  zh <- newIORef 90
  fv <- newIORef 65
  as <- newIORef 1
  di <- newIORef 2
  i  <- newIORef ("","")
  return $ State {  
    frames = f, t0 = t, ph' = ph, th' = th, gr' = gr, zh' = zh, asp = as, fov = fv, dim = di, info = i
  }

type Point3 = (Float, Float, Float)
--type Point4 = (Float, Float, Float, Float)
data Point4 = Point4 Float Float Float Float deriving (Show, Eq)

type Scale      = Maybe Float
type Paint      = Maybe Point4
type Location   = Maybe Point3
type NoseVector = Maybe Point3
type UpVector   = Maybe Point3
type Ambience4  = Maybe Point4
type Diffuse4   = Maybe Point4
type Specular4  = Maybe Point4
type Emission4  = Maybe Point4
type Shininess  = Maybe Int

--type ObjectAttributes = (Scale, Paint, Location, NoseVector, UpVector, Ambience4, Diffuse4, Specular4, Shininess)
data ObjectAttributes = ObjectAttributes {
  scaleSize  :: Scale,
  paint      :: Paint,
  location   :: Location,
  noseVector :: NoseVector,
  upVector   :: UpVector,
  ambience4  :: Ambience4,
  diffuse4   :: Diffuse4,
  specular4  :: Specular4,
  emission4  :: Emission4,
  shininess  :: Shininess
} deriving (Show, Eq)


--toGfloat :: Float -> GLfloat
--toGfloat f = (realToFrac f)::GLfloat

iToGL :: Int -> GLfloat
iToGL i = (fromIntegral i)::GLfloat

fToGL :: Float -> GLfloat
fToGL f = (realToFrac f)::GLfloat

glCos :: Float -> Float
glCos x = cos(3.1415927/180*x)

glSin :: Float -> Float
glSin x = sin(3.1415927/180*x)

toDeg :: Float -> Float
toDeg x = x*(3.1415927/180)

drawNormal3f :: Float -> Float -> Float -> IO ()
drawNormal3f x y z = currentNormal $= Normal3 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat)

drawVertex3f :: Float -> Float -> Float -> IO ()
drawVertex3f x y z = vertex $ vertex3f x y z

vertex3f :: Float -> Float -> Float -> Vertex3 GLfloat
vertex3f x y z = Vertex3 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat)

vertex3d :: Float -> Float -> Float -> Vertex3 GLdouble
vertex3d x y z = Vertex3 ((realToFrac x)::GLdouble) ((realToFrac y)::GLdouble) ((realToFrac z)::GLdouble)

vertex4f :: Float -> Float -> Float -> Float -> Vertex4 GLfloat
vertex4f x y z w = Vertex4 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac w)::GLfloat)

vector3f :: Float -> Float -> Float -> Vector3 GLfloat
vector3f x y z = Vector3 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat)

vector3d :: Float -> Float -> Float -> Vector3 GLdouble
vector3d x y z = Vector3 ((realToFrac x)::GLdouble) ((realToFrac y)::GLdouble) ((realToFrac z)::GLdouble)

scale3f :: Float -> Float -> Float -> IO ()
scale3f x y z = scale ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat)

color3f :: Float -> Float -> Float -> IO ()
color3f x y z = color (Color3 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat))

color4f :: Point4 -> IO ()
color4f (Point4 x y z a) = color (Color4  ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac a)::GLfloat))

setPerspective :: Float -> Float -> Float -> Float -> IO ()
setPerspective fov aspect zNear zFar = perspective ((realToFrac fov)::GLdouble) ((realToFrac aspect)::GLdouble) ((realToFrac zNear)::GLdouble) ((realToFrac zFar)::GLdouble)

setOrtho :: Float -> Float -> Float -> Float -> Float -> Float -> IO ()
setOrtho left right bottom top nearVal farVal = ortho ((realToFrac left)::GLdouble) ((realToFrac right)::GLdouble) ((realToFrac bottom)::GLdouble) ((realToFrac top)::GLdouble) ((realToFrac nearVal)::GLdouble) ((realToFrac farVal)::GLdouble)

setLookAt :: Point3 -> Point3 -> Point3 -> IO ()
setLookAt (ex,ey,ez) (cx,cy,cz) (ux,uy,uz) = lookAt (vertex3d ex ey ez) (vertex3d cx cy cz) (vector3d ux uy uz)

ambient4f :: Point4 -> IO ()
ambient4f (Point4 x y z a) = ambient (Light 0) $= Color4 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac a)::GLfloat)

specular4f :: Point4 -> IO ()
specular4f (Point4 x y z a) = specular (Light 0) $= Color4 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac a)::GLfloat)

diffuse4f :: Point4 -> IO ()
diffuse4f (Point4 x y z a) = diffuse (Light 0) $= Color4 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac a)::GLfloat)

position4f :: Point4 -> IO ()
position4f (Point4 x y z a) = position (Light 0) $= Vertex4 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac a)::GLfloat)

pointToColor4f :: Point4 -> Color4 GLfloat
pointToColor4f (Point4 x y z a) = Color4 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac a)::GLfloat)

glWindowPos :: GLfloat -> GLfloat -> IO ()
glWindowPos x y = glWindowPos2f x y

round2 :: Float -> String
round2 x = showFFloat (Just 2) x ""

round2GL :: GLfloat -> String
round2GL x = showGFloat (Just 2) x ""

listf :: [Float] -> [GLfloat]
listf ls = map (\x -> ((realToFrac x)::GLfloat)) ls