import Control.Monad ( when )
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

keyboard state (Char 'z')           _ _ _ = modDim state Decrease
keyboard state (Char 'Z')           _ _ _ = modDim state Increase

keyboard state (Char 'f')           _ _ _ = modFov state Decrease
keyboard state (Char 'F')           _ _ _ = modFov state Increase

keyboard state (Char '[')           _ _ _ = modLightHeight state Decrease
keyboard state (Char ']')           _ _ _ = modLightHeight state Increase

keyboard state (Char 'r')           _ _ _ = modLightRadius state Decrease
keyboard state (Char 'R')           _ _ _ = modLightRadius state Increase

keyboard state (Char 's')           _ _ _ = modSpecular state Decrease
keyboard state (Char 'S')           _ _ _ = modSpecular state Increase

keyboard state (Char 'd')           _ _ _ = modDiffusion state Decrease
keyboard state (Char 'D')           _ _ _ = modDiffusion state Increase

keyboard state (Char 'a')           _ _ _ = modAmbience state Decrease
keyboard state (Char 'A')           _ _ _ = modAmbience state Increase

keyboard state (Char 'n')           _ _ _ = modShininess state Decrease
keyboard state (Char 'N')           _ _ _ = modShininess state Increase


keyboard state (Char 'l')           keyState _ _ = toggleLight state keyState
keyboard state (Char 'h')           keyState _ _ = toggleShading state keyState

keyboard _     (Char '\27')         _ _ _ = exitWith ExitSuccess
keyboard _     _                    _ _ _ = return ()


toggleShading :: State -> KeyState -> IO ()
toggleShading state  Up = do
  smoothStatus <- get (smooth' state)
  if smoothStatus == Smooth
    then smooth' state $~! (\x -> Flat)
    else smooth' state $~! (\x -> Smooth)
toggleShading state Down = postRedisplay Nothing


toggleLight :: State -> KeyState -> IO ()
toggleLight state  Up = do
  lightStatus <- get (light' state)
  if lightStatus
    then light' state $~! (\x -> False)
    else light' state $~! (\x -> True)
toggleLight state Down = postRedisplay Nothing


modShininess :: State -> ChangeDirection -> IO ()
modShininess state Decrease = do
  shine' state $~! (\x -> x - 1)
modShininess state Increase  = do
  shine' state $~! (+1)  

modAmbience :: State -> ChangeDirection -> IO ()
modAmbience state Decrease = do
  amb' state $~! (\x -> x - 5)
modAmbience state Increase  = do
  amb' state $~! (+5)

modDiffusion :: State -> ChangeDirection -> IO ()
modDiffusion state Decrease = do
  diff' state $~! (\x -> x - 5)
modDiffusion state Increase  = do
  diff' state $~! (+5)

modSpecular :: State -> ChangeDirection -> IO ()
modSpecular state Decrease = do
  spec' state $~! (\x -> x - 5)
modSpecular state Increase  = do
  spec' state $~! (+5)

modLightRadius :: State -> ChangeDirection -> IO ()
modLightRadius state Decrease = do
  rlight' state $~! (\x -> x - 1)
modLightRadius state Increase  = do
  rlight' state $~! (+1)

modLightHeight :: State -> ChangeDirection -> IO ()
modLightHeight state Decrease = do
  ylight' state $~! (\x -> x - 0.1)
modLightHeight state Increase  = do
  ylight' state $~! (+0.1)

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

  spec <- get (spec' state)
  amb <- get (amb' state)
  diff <- get (diff' state)
  shine <- get (shine' state)
  lightStatus <- get (light' state)

  t <- get elapsedTime

  let seconds = ((fromIntegral t))/1000.0
  zh' state $~! (\x -> mod' (90*seconds) 360)

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

  if spec > 100
    then spec' state $~! (\x -> 100)
    else if spec < 0
      then spec' state $~! (\x -> 0)
      else postRedisplay Nothing

  if diff > 100
    then diff' state $~! (\x -> 100)
    else if diff < 0
      then diff' state $~! (\x -> 0)
      else postRedisplay Nothing

  if amb > 100
    then amb' state $~! (\x -> 100)
    else if amb < 0
      then amb' state $~! (\x -> 0)
      else postRedisplay Nothing

  if shine > 100
    then shine' state $~! (\x -> 100)
    else if shine < 0
      then shine' state $~! (\x -> 0)
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
    
    spec <- get (spec' state)
    amb <- get (amb' state)
    diff <- get (diff' state)
    shine <- get (shine' state)
    lightStatus <- get (light' state)
    shadStatus <- get (smooth' state)

    let seconds = fromIntegral (t - t0') / 1000 :: GLfloat
        fps = fromIntegral f / seconds
        result = ("[ph " ++ round2 ph ++ "] [th " ++ round2 th ++ "] [dim " ++ show dim ++ "] [lightStatus " ++ show lightStatus ++  "] ",
                  "[specular " ++ show spec ++  "] [ambience " ++ show amb ++  "] [ diffuse " ++ show diff ++  "] [shininess " ++ show shine ++  "] [shading " ++ show shadStatus ++  "] ")
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
  
  ylight <- get (ylight' state)
  rlight <- get (rlight' state)
  ambience <- get (amb' state)
  diffusion <- get (diff' state)
  specularizion <- get (spec' state)
  emission <- get (emiss' state)
  shine'   <- get (shine' state)

  lightStatus <- get (light' state)
  shadeStatus <- get (smooth' state)


  loadIdentity


  let ex = (-2)*dim*sin(toDeg(th))*cos(toDeg(ph))
      ey =    2*dim               *sin(toDeg(ph))
      ez =    2*dim*cos(toDeg(th))*cos(toDeg(ph))
  setLookAt (ex,ey,ez) (0,0,0) (0,cos(toDeg(ph)),0)


  ------------------------------------
  --shadeModel $= Smooth

  let ambs     = (Point4 (0.01*ambience) (0.01*ambience) (0.01*ambience) 1.0)
      diffs    = (Point4 (0.01*diffusion) (0.01*diffusion) (0.01*diffusion) 1.0)
      specs    = (Point4 (0.01*specularizion) (0.01*specularizion) (0.01*specularizion) 1.0)
      loc3     = (rlight*glCos(zh), ylight, rlight*glSin(zh))
      loc4     = (Point4 (rlight*glCos(zh)) ylight (rlight*glSin(zh)) 1.0)
      yellow   = (Point4 1.0 1.0 0.0 1.0)
      white    = (Point4 1 1 1 1)
      black    = (Point4 0 0 0 1)
      emiss    = (Point4 0.0 0.0 (0.01*emission) 1.0)


  if lightStatus
    then do
      normalize $= Enabled
      lighting $= Enabled
      lightModelLocalViewer $= Enabled
      colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
      light (Light 0) $= Enabled

      shadeModel $= shadeStatus

      ambient4f ambs
      specular4f specs
      diffuse4f diffs
      position4f loc4
    else do
      lighting $= Disabled


    

  drawSphere state $ ObjectAttributes {  
    scaleSize  = (Just 0.5),
    paint      = Just $ (Point4 255 255 0 0),
    location   = (Just loc3),
    noseVector = Nothing,
    upVector   = Nothing,
    ambience4  = Nothing,
    diffuse4   = Nothing,
    specular4  = Nothing,
    emission4  = Nothing,
    shininess  = Nothing
  }

  drawCube state $ ObjectAttributes {  
    scaleSize  = (Just 0.5),
    paint      = Just $ (Point4 1 0 1 0),
    location   = (Just ((-1.5), 0, 0)),
    noseVector = Nothing,
    upVector   = Nothing,
    ambience4  = Nothing,
    diffuse4   = Nothing,
    specular4  = Just white,
    emission4  = Just black,
    shininess  = Just shine'
  }
  
  drawStation state (fToGL gr) 0.3 (1.5,0,0)

  drawStation state (fToGL (gr*1.2)) 0.3 (1,1,1)

  drawSphere state $ ObjectAttributes {  
    scaleSize  = (Just 0.5),
    paint      = Just $ (Point4 1 1 1 1),
    location   = (Just (0, 0, 0)),
    noseVector = Nothing,
    upVector   = Nothing,
    ambience4  = Nothing,
    diffuse4   = Nothing,
    specular4  = Nothing,
    emission4  = Just $ (Point4 0 0 (0.01*emission) 1),
    shininess  = Nothing
  }

  drawFighter 1.0 (1, 0, 1) (0,1,0) (0, 1,0)

  

  lighting $= Disabled
  ------------------------------------
  
  drawGrid 5


  
  --drawStar 0.5 (0, 1.5, 0)

  --drawStarCluster (5, 1, 3)
  --drawStarCluster (5, 5, 1)
  --drawStarCluster (1, 5, 5)

  --drawStation 0.0 0.5 (1,0,0) (0,1,0)

  
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
    --keyboardUpCallback $= Just (keyboard state)
    visibilityCallback $= Just (visible state)
    mainLoop
  


