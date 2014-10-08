import Control.Monad ( when )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )
import Data.Fixed

import Graphics.UI.GLUT

import GLUtils
import Cube
import Star
import Grid
import StarCluster
import Fighter
import Pyramid
import Station
import Sphere

----------------------------------------------------------------------------------------------------------------
-- Global State

data ChangeDirection = Increase | Decrease deriving (Show)

data ProjectionView = PerspectiveView | OrthogonalView | FirstPersonView deriving (Show, Eq)

data Direction = UpDirection | DownDirection | LeftDirection | RightDirection deriving (Show, Eq)

fraction  = 0.1

--lightEnabled =   True --  Lighting
one       =   1    -- Unit value
distance  =   5    -- Light distance
inc       =  10    -- Ball increment
smooth    =   1    -- Smooth/Flat shading
local     =   0    -- Local Viewer Model
emission  =   0    -- Emission intensity (%)
ambience   =  30    -- Ambient intensity (%)
diffusion   = 100    -- Diffuse intensity (%)
specularizion  =   0    -- Specular intensity (%)
shininess =   0    -- Shininess (power of two)
--zh        =  90    -- Light azimuth
ylight    =   0    -- Elevation of light
--shinyvec[1]        -- Shininess (value)  


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

----------------------------------------------------------------------------------------------------------------
-- Timer 
timerFrequencyMillis :: Timeout
timerFrequencyMillis = 20

timer :: State -> TimerCallback
timer state = do
  addTimerCallback timerFrequencyMillis (timer state)

----------------------------------------------------------------------------------------------------------------
-- Key Binding
keyboard :: State -> KeyboardMouseCallback
keyboard state (SpecialKey KeyUp)   _ _ _ = modRotate state KeyUp
keyboard state (SpecialKey KeyDown) _ _ _ = modRotate state KeyDown
keyboard state (SpecialKey KeyLeft) _ _ _ = modRotate state KeyLeft
keyboard state (SpecialKey KeyRight)_ _ _ = modRotate state KeyRight

keyboard state (Char 'd')           _ _ _ = modDim state Decrease
keyboard state (Char 'D')           _ _ _ = modDim state Increase

keyboard state (Char 'f')           _ _ _ = modFov state Decrease
keyboard state (Char 'F')           _ _ _ = modFov state Increase

keyboard _     (Char '\27')         _ _ _ = exitWith ExitSuccess
keyboard _     _                    _ _ _ = return ()


modRotate :: State -> SpecialKey -> IO ()
modRotate state KeyDown = do
  ph' state $~! (\x -> x - 5)
modRotate state KeyUp  = do
  ph' state $~! (+5)
modRotate state KeyRight = do
  th' state $~! (\x -> x - 5)
modRotate state KeyLeft = do
  th' state $~! (+5)


modFov :: State -> ChangeDirection -> IO ()
modFov state Decrease = do
  fov state $~! (\x -> x - 2)
  postRedisplay Nothing
modFov state Increase = do
  fov state $~! (+2)
  postRedisplay Nothing  


modDim :: State -> ChangeDirection -> IO ()
modDim state Decrease = do
  dim state $~! (\x -> x - 0.1)
  postRedisplay Nothing
modDim state Increase = do
  dim state $~! (+0.1)
  postRedisplay Nothing  


idle :: State -> IdleCallback
idle state = do

  ph  <- get (ph' state)
  th  <- get (th' state)
  gr  <- get (gr' state)
  zh  <- get (zh' state)
  dim' <- get (dim state)
  fov' <- get (fov state)
  t <- get elapsedTime

  let seconds = ((fromIntegral t))/1000.0

  --double t = glutGet(GLUT_ELAPSED_TIME)/1000.0;
  --zh = fmod(90*t,360.0);


  zh' state $~! (\x -> mod' (90*seconds) 360)
  --zh  <- get (zh' state)
  --putStrLn $ show zh

  if fov' < 55
    then fov state $~! (\x -> 55)
    else postRedisplay Nothing

  if dim' < 1
    then dim state $~! (\x -> 1)
    else postRedisplay Nothing

  if gr > 360
    then gr' state $~! (\x -> 0)
    else gr' state $~! (+2)
  
  if ((-360) > ph || ph > 360)
    then ph' state $~! (\x -> 0)
    else postRedisplay Nothing

  if ((-360) > th || th > 360)
    then th' state $~! (\x -> 0)
    else postRedisplay Nothing

  postRedisplay Nothing


visible :: State -> Visibility -> IO ()
visible state Visible    = idleCallback $= Just (idle state)
visible _     NotVisible = idleCallback $= Nothing
  
reshape :: State -> ReshapeCallback
reshape state s@(Size width height) = do


  if height > 0
    then asp state $~! (\x -> (fromIntegral width)/(fromIntegral height))
    else asp state $~! (\x -> 1)

  viewport   $= (Position 0 0, s)

  matrixMode $= Projection
  loadIdentity

  fov <- get (fov state)
  asp <- get (asp state)
  dim <- get (dim state)
  setPerspective fov asp (dim/16) (dim*16)

  matrixMode $= Modelview 0
  loadIdentity


----------------------------------------------------------------------------------------------------------------
-- Debug info
updateInfo :: State -> IO ()
updateInfo state = do 
  frames state $~! (+1)
  t0' <- get (t0 state)
  t <- get elapsedTime
  when (t - t0' >= 1000) $ do
    f <- get (frames state)
    ph <- get (ph' state)
    th <- get (th' state)
    gr <- get (gr' state)
    asp <- get (asp state)
    fov <- get (fov state)
    dim <- get (dim state)
    let seconds = fromIntegral (t - t0') / 1000 :: GLfloat
        fps = fromIntegral f / seconds
        result = ("[ph " ++ round2 ph ++ "] [th " ++ round2 th ++ "] [gr " ++ round2 gr ++ "]",
                  "[asp " ++ show asp ++  "] [fov " ++ show fov ++  "] [ dim" ++ show dim ++  "] ")
    info state $= result
    t0 state $= t
    frames state $= 0


draw :: State -> IO ()
draw state = do

  clear [ ColorBuffer, DepthBuffer ]

  ph <- get (ph' state)
  th <- get (th' state)
  gr <- get (gr' state)
  zh  <- get (zh' state)
  dim <- get (dim state)
  info <- get (info state)

  loadIdentity


  let ex = (-2)*dim*sin(toDeg(th))*cos(toDeg(ph))
      ey =    2*dim               *sin(toDeg(ph))
      ez =    2*dim*cos(toDeg(th))*cos(toDeg(ph))
  setLookAt (ex,ey,ez) (0,0,0) (0,cos(toDeg(ph)),0)


  ------------------------------------
  shadeModel $= Smooth

  let ambs  = (0.01*ambience ,    0.01*ambience ,    0.01*ambience ,    1.0)
      diffs = (0.01*diffusion ,   0.01*diffusion ,   0.01*diffusion ,   1.0)
      specs = (0.01*specularizion,0.01*specularizion,0.01*specularizion,1.0)
      position = (distance*glCos(zh),ylight,distance*glSin(zh))
      yellow   = [1.0,1.0,0.0,1.0]
      emiss = [0.0,0.0,0.01*emission,1.0]


  normalize $= Enabled
  lighting $= Enabled
  lightModelLocalViewer $= Enabled
  colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
  light (Light 0) $= Enabled

  drawStar 0.5 position

  









  lighting $= Disabled
  ------------------------------------
  
  --drawGrid 5

  --drawCube 1.5 ((-1),0,0)
  --drawPyramid 0.5 (1.5,0,0) (1,0,0) (0,1,0)

  --drawSphere 0.5 0.5 (0,0,0)
  
  --drawStar 0.5 (0, 1.5, 0)

  --drawStarCluster (5, 1, 3)
  --drawStarCluster (5, 5, 1)
  --drawStarCluster (1, 5, 5)

  --drawStation 0.0 0.5 (1,0,0) (0,1,0)

  --drawFighter 0.5 (0.55, 0, 0) (0,1,0) ((-1), 0,0)
  --drawFighter 0.7 (1, 0.7, 0) (1,0,0) (0,1,0)
  --drawFighter 0.5 (0,1,1) (1,1,1) (0,1,0)

  preservingMatrix $ do
    glWindowPos 5 30
    renderString Helvetica18 $ (fst info)
    glWindowPos 5 5
    renderString Helvetica18 $ (snd info)

  swapBuffers
  updateInfo state
  reportErrors
  

myInit :: [String] -> State -> IO ()
myInit args state = do
  clearColor $= Color4 0 0 0 0
  depthFunc $= Just Less
  --materialDiffuse Front $= Color4 0.7 0.7 0.7 1
  --materialSpecular Front $= Color4 1 1 1 1
  --materialShininess Front $= 100
  
  --normalize $= Enabled
  --lighting $= Enabled
  --lightModelLocalViewer $= Enabled
  --colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
  --light (Light 0) $= Enabled
  
  --autoNormal $= Enabled
  
  

main :: IO ()
main = do
    initialWindowSize $= Size 800 800
    (_progName, args) <- getArgsAndInitialize
    initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
    
    --initialWindowPosition $= Position 500 500
    _window <- createWindow "Space Scene Projection - Adam Cardenas"

    state <- makeState
    myInit args state

    displayCallback $= draw state
    reshapeCallback $= Just (reshape state)
    
    keyboardMouseCallback $= Just (keyboard state)
    visibilityCallback $= Just (visible state)
    mainLoop
  


