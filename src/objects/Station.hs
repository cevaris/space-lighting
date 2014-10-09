module Station (drawStation) where 
 
import Graphics.UI.GLUT

import GLUtils

import Cube
import Pyramid
  
drawStation :: State ->
               GLfloat ->
               Float->
               (Float, Float, Float) ->
               (Float, Float, Float) -> IO ()
drawStation state a s (x, y, z) (ux, uy, uz) = do

    let ws = s*0.6
        wd = s*2
        cs = s*1.5
        yellow   = (Point4 1.0 1.0 0.0 1.0)
        white    = (Point4 1 1 1 1)
        black    = (Point4 0 0 0 1)
        shininess' = 0
    
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do

        translate $ vector3f x y z
        scale3f s s s
        rotate a (Vector3 0 1 0)

        drawCube state $ ObjectAttributes {  
          scaleSize  = Just cs,
          paint      = Just $ (Point4 1 1 1 0),
          location   = Just $ (0, 0, 0),
          noseVector = Nothing,
          upVector   = Nothing,
          ambience4  = Nothing,
          diffuse4   = Nothing,
          specular4  = Just white,
          emission4  = Just black,
          shininess  = Just shininess'
        }

        -- Bottom
        --drawPyramid s (0,(-0.75),0) (1,0,0) (0,1,0)
        drawPyramid state $ ObjectAttributes {
          scaleSize  = Just $ s,
          paint      = Nothing,
          location   = Just $ (0,(-0.75),0),
          noseVector = Just $ (1,0,0),
          upVector   = Just $ (0,1,0),
          ambience4  = Nothing,
          diffuse4   = Nothing,
          specular4  = Just white,
          emission4  = Just black,
          shininess  = Just shininess'
        }
        -- Top
        --drawPyramid s (0,0.75,0) (1,0,0) (0,(-1),0)
        drawPyramid state $ ObjectAttributes {
          scaleSize  = Just $ s,
          paint      = Nothing,
          location   = Just $ (0,0.75,0),
          noseVector = Just $ (1,0,0),
          upVector   = Just $ (0,(-1),0),
          ambience4  = Nothing,
          diffuse4   = Nothing,
          specular4  = Just white,
          emission4  = Just black,
          shininess  = Just shininess'
        }
        ---- Front
        ----drawPyramid ws (0,0,wd) (1,0,0) (0,0,(-1))
        --drawPyramid state $ ObjectAttributes {
        --  scaleSize  = Just $ ws,
        --  paint      = Nothing,
        --  location   = Just $ (0,0,wd),
        --  noseVector = Just $ (1,0,0),
        --  upVector   = Just $ (0,0,(-1)),
        --  ambience4  = Nothing,
        --  diffuse4   = Nothing,
        --  specular4  = Just white,
        --  emission4  = Just black,
        --  shininess  = Just shininess'
        --}
        ---- Back
        ----drawPyramid ws (0,0,(-wd)) (1,0,0) (0,0,1)
        --drawPyramid state $ ObjectAttributes {
        --  scaleSize  = Just $ ws,
        --  paint      = Nothing,
        --  location   = Just $ (0,0,(-wd)),
        --  noseVector = Just $ (1,0,0),
        --  upVector   = Just $ (0,0,1),
        --  ambience4  = Nothing,
        --  diffuse4   = Nothing,
        --  specular4  = Just white,
        --  emission4  = Just black,
        --  shininess  = Just shininess'
        --}
        ---- Right
        ----drawPyramid ws (wd,0,0) (0,1,0) ((-1),0,0)
        --drawPyramid state $ ObjectAttributes {
        --  scaleSize  = Just $ ws,
        --  paint      = Nothing,
        --  location   = Just $ (wd,0,0),
        --  noseVector = Just $ (0,1,0),
        --  upVector   = Just $ (0,1,0),
        --  ambience4  = Nothing,
        --  diffuse4   = Nothing,
        --  specular4  = Just white,
        --  emission4  = Just black,
        --  shininess  = Just shininess'
        --}
        ------ Front
        --drawPyramid ws ((-wd),0,0) (0,1,0) (1,0,0)
        





        --drawCube cs (0,0,0)
        -- Up
        --drawPyramid s (0,(-0.75),0) (1,0,0) (0,1,0)
        -- Down
        --drawPyramid s (0,0.75,0) (1,0,0) (0,(-1),0)
        
        -- Front
        --drawPyramid ws (0,0,wd) (1,0,0) (0,0,(-1))
        -- Back
        --drawPyramid ws (0,0,(-wd)) (1,0,0) (0,0,1)
        -- Right
        --drawPyramid ws (wd,0,0) (0,1,0) ((-1),0,0)
        ---- Front
        --drawPyramid ws ((-wd),0,0) (0,1,0) (1,0,0)
        
            
      