module Fighter (drawFighter) where 
 
import Graphics.UI.GLUT

import GLUtils
  


--Draw solid airplane
--  scale (s)
--  at (x,y,z)
--  nose towards (dx,dy,dz)
--  up towards (ux,uy,uz)
drawFighter :: State -> ObjectAttributes -> IO ()
drawFighter state object@(ObjectAttributes scaleSize paint location noseVector upVector _ _ _ _ _) = do
  

  case (location, noseVector, upVector, scaleSize, paint) of
    ((Just (lx, ly, lz)), (Just (dx, dy, dz)), (Just (ux, uy, uz)), (Just s), (Just (Point4 cx cy cz ca))) -> do 

    let wid  = 0.05
        nose = 0.50
        cone = 0.20
        wing = 0.00
        strk = (-0.20)
        tail = (-0.50)
        d0 = sqrt(dx*dx+dy*dy+dz*dz)
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


    drawLightingEffects object


    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        -- Offset, scale and rotate
        color3f cx cy cz
        translate $ vector3f lx ly lz
        scale3f s s s
        multMatrix (mat :: GLmatrix GLfloat)

        renderPrimitive Triangles $ do

          -- Front
          drawNormal3f 1 0 (cone/wid)
          drawVertex3f nose 0 0
          drawVertex3f cone wid wid
          drawVertex3f cone (-wid) wid

          -- Back
          drawNormal3f 1 0 (-cone/wid)
          drawVertex3f nose  0.0  0.0
          drawVertex3f cone  wid (-wid)
          drawVertex3f cone (-wid) (-wid)

          -- Top
          drawNormal3f 1 (cone/wid) 0
          drawVertex3f nose  0.0  0.0
          drawVertex3f cone  wid  wid
          drawVertex3f cone  wid (-wid)

          -- Cockpit
          color3f (0/255)  (0/255)  (0/255) 
          drawNormal3f 1 (cone/wid) 0
          drawVertex3f nose  0.0  0.0
          drawVertex3f (cone*1.1)  (wid*1.05)  (wid*0.4)
          drawVertex3f (cone*1.1)  (wid*1.05) (-(wid*0.4))
          color3f cx cy cz

          -- Bottom
          drawNormal3f 1 (-cone/wid) 0
          drawVertex3f nose  0.0  0.0
          drawVertex3f cone (-wid) (wid)
          drawVertex3f cone (-wid) (-wid)

        
        renderPrimitive Quads $ do
          -- Front
          drawNormal3f 0 0 1
          drawVertex3f cone  wid  wid
          drawVertex3f cone (-wid)  wid
          drawVertex3f tail (-wid)  wid
          drawVertex3f tail  wid  wid

          -- Back
          drawNormal3f 0 0 (-1)
          drawVertex3f cone  wid (-wid)
          drawVertex3f cone (-wid) (-wid)
          drawVertex3f tail (-wid) (-wid)
          drawVertex3f tail  wid (-wid)

          -- Top
          drawNormal3f 0 1 0
          drawVertex3f cone  wid  wid
          drawVertex3f cone  wid (-wid)
          drawVertex3f tail  wid (-wid)
          drawVertex3f tail  wid  wid

          -- Bottom
          drawNormal3f 0 (-1) 0
          drawVertex3f cone (-wid)  wid
          drawVertex3f cone (-wid) (-wid)
          drawVertex3f tail (-wid) (-wid)
          drawVertex3f tail (-wid)  wid

          -- Tail Cap
          drawNormal3f (-1) 0 0
          drawVertex3f tail (-wid)  wid
          drawVertex3f tail  wid  wid
          drawVertex3f tail  wid (-wid)
          drawVertex3f tail (-wid) (-wid)

        --color3f 1 1 0
        --color3f (211/255) (211/255) (211/255)
        --color3f 1 0 0
        color3f cx cy cz
        renderPrimitive Triangles $ do

          drawNormal3f 0 1 0
          drawVertex3f wing 0.0  wid
          drawVertex3f tail 0.0  wid
          drawVertex3f tail 0.0  0.5

          drawNormal3f 0 1 0
          drawVertex3f wing 0.0 (-wid)
          drawVertex3f tail 0.0 (-wid)
          drawVertex3f tail 0.0 (-0.5)

        --color3f 1 0 0
        color3f cx cy cz
        --color3f (211/255) (211/255) (211/255)
        renderPrimitive Triangles $ do
          drawNormal3f 0 0 1
          drawVertex3f strk 0.0 0.0001
          drawVertex3f tail 0.3 0.0001
          drawVertex3f tail 0.0 0.0001

          drawNormal3f 0 0 (-1)
          drawVertex3f strk 0.0 (-0.0001)
          drawVertex3f tail 0.3 (-0.0001)
          drawVertex3f tail 0.0 (-0.0001)
        




      
        
      
