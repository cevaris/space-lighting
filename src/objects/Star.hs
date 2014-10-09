module Star (drawStar) where 
 
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects

import GLUtils

import Sphere

-- Draw solid pyramid
--  scale (s)
--  at (x,y,z)
--drawStar :: Float-> (Float, Float, Float) -> IO ()
--drawStar s (x, y, z) = do
drawStar :: ObjectAttributes -> IO ()
drawStar object@(ObjectAttributes scaleSize paint location noseVector upVector ambience4 diffuse4 specular4 shininess) = do
  
  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do  

      case (paint, location, scaleSize) of
        ((Just (px, py, pz)), (Just (lx, ly, lz)), (Just s))-> do 
          color3f px py pz
          translate $ vector3f lx ly lz
          scale3f s s s
          
          --drawSphere s 0.5 (0,0,0)
          drawSphere object
        _ -> putStrLn $ "Start Light case Fail: " ++ (show object)