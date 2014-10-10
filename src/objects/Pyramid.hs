module Pyramid (drawPyramid) where 
 
import Graphics.UI.GLUT

import GLUtils

-- Draw solid pyramid
----  scale (s)
----  at (x,y,z)
----  nose towards (dx,dy,dz)
----  up towards (ux,uy,uz)
--drawPyramid :: Float -> 
--               (Float, Float, Float) -> 
--               (Float, Float, Float) ->
--               (Float, Float, Float) -> IO ()
--drawPyramid s (x, y, z) (dx, dy, dz) (ux, uy, uz) = do
drawPyramid :: State -> ObjectAttributes -> IO ()
drawPyramid state object@(ObjectAttributes scaleSize paint location noseVector upVector _ _ _ _ _) = do

  case (location, noseVector, upVector, scaleSize, paint) of
    ((Just (lx, ly, lz)), (Just (dx, dy, dz)), (Just (ux, uy, uz)), (Just s), (Just (Point4 cx cy cz ca))) -> do 

      let d0 = sqrt(dx*dx+dy*dy+dz*dz)
          x0 = dx/d0
          y0 = dy/d0
          z0 = dz/d0
          --  Unit vector in "up" direction
          d1 = sqrt(ux*ux+uy*uy+uz*uz)
          x1 = ux/d1
          y1 = uy/d1
          z1 = uz/d1
          -- Cross product gives the third vector
          x2 = y0*z1-y1*z0
          y2 = z0*x1-z1*x0
          z2 = x0*y1-x1*y0
      mat <- newMatrix RowMajor $ listf [x0, x1,  x2, 0,
                                         y0, y1,  y2, 0,
                                         z0, z1,  z2, 0,
                                         0,  0,   0,  1]

      preservingMatrix $ do
        preservingAttrib [AllServerAttributes] $ do
          
          color3f cx cy cz
          translate $ vector3f lx ly lz
          scale3f s s s
          multMatrix (mat :: GLmatrix GLfloat)

          drawLightingEffects object

          renderPrimitive Triangles $ do
            
            -- Front
            drawNormal3f 0 0.5 0.5
            drawVertex3f  0 1 0
            drawVertex3f (-1) (-1) 1
            drawVertex3f 1 (-1) 1
       
            -- Right
            drawNormal3f 0.5 0.5 0
            drawVertex3f 0 1 0
            drawVertex3f 1 (-1) 1
            drawVertex3f 1 (-1) (-1)
       
            -- Back
            drawNormal3f 0 0.5 (-0.5)
            drawVertex3f 0 1 0
            drawVertex3f 1 (-1) (-1)
            drawVertex3f (-1) (-1) (-1)
       
            -- Left
            drawNormal3f (-0.5) 0.5 0
            drawVertex3f  0 1 0
            drawVertex3f (-1)(-1)(-1)
            drawVertex3f (-1) (-1) 1

          renderPrimitive Quads $ do

            color3f 0 0 1
            
            drawNormal3f 0 (-0.5) 0
            drawVertex3f 1 (-1) 1
            drawVertex3f 1 (-1) (-1)
            drawVertex3f (-1) (-1) (-1)
            drawVertex3f (-1) (-1) 1

            
        
              
        