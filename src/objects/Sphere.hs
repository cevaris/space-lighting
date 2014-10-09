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

  let yellow = (Point4 1.0 1.0 0.0 1.0)

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      let q = 0.5

      case (paint, location, scaleSize) of
        ((Just (Point4 px py pz pa)), (Just (lx, ly, lz)), (Just s))-> do 
          color3f px py pz
          translate $ vector3f lx ly lz
          scale3f s s s

          case specular4 of 
            (Just point4) -> do 
              materialSpecular Front $= pointToColor4f point4
            _ -> postRedisplay Nothing

          case emission4 of 
            (Just point4) -> do 
              materialEmission Front $= pointToColor4f point4
            _ -> postRedisplay Nothing

          case shininess of 
            (Just sh) -> do 
              materialShininess Front $= (iToGL sh)
            _ -> postRedisplay Nothing
          

          mapM_ (\ph -> do
              renderPrimitive QuadStrip $ mapM_ (\th -> drawLatBand q (ph, th)) (sphereTh q)
            ) (spherePh q)
          
          --drawSphere s 0.5 (0,0,0)
        _ -> putStrLn $ "Start Light case Fail: " ++ (show object)

      


      --translate $ vector3f x y z
      --scale3f s s s

      --mapM_ (\ph -> do
      --    renderPrimitive QuadStrip $ mapM_ (\th -> drawLatBand q (ph, th)) (sphereTh q)
      --  ) (spherePh q)

        
            
      