module Sphere (drawSphere) where 
 
import Graphics.UI.GLUT
import Data.Fixed

import GLUtils

spherePh:: Float -> [Float]
spherePh d = [ ph | ph <- [(-90.0)..90.0], ((mod' ph d) == 0 && ph < 90)]

sphereTh:: Float -> [Float]
sphereTh d = [th | th <- [0.0..360.0], (mod' th d) == 0]


drawLatBand :: Float -> (Float,Float) -> IO ()
drawLatBand d (ph, th) =  do

  drawNormal3f ((sin th)*(cos ph)) (sin ph) ((cos th)*(cos ph))
  drawVertex3f ((sin th)*(cos ph)) (sin ph) ((cos th)*(cos ph))

  drawNormal3f ((sin th)*(cos (ph+d))) (sin (ph+d)) ((cos th)*(cos (ph+d)))
  drawVertex3f ((sin th)*(cos (ph+d))) (sin (ph+d)) ((cos th)*(cos (ph+d)))
  

--Draw solid sphere
drawSphere :: State -> ObjectAttributes -> IO ()
drawSphere state object@(ObjectAttributes scaleSize paint location noseVector upVector ambience4 diffuse4 specular4 emission4 shininess) = do

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      let q = 0.25

      case (paint, location, scaleSize) of
        ((Just (Point4 px py pz pa)), (Just (lx, ly, lz)), (Just s))-> do 
          color3f px py pz
          translate $ vector3f lx ly lz
          scale3f s s s

          drawLightingEffects object

          mapM_ (\ph -> do
              renderPrimitive QuadStrip $ mapM_ (\th -> drawLatBand q (ph, th)) (sphereTh q)
            ) (spherePh q)