module Cube (drawCube) where 
 
import Graphics.UI.GLUT

import GLUtils


--drawCube :: Float -> (Float, Float, Float) -> IO ()
--drawCube s (x, y, z) = do
drawCube :: State -> ObjectAttributes -> IO ()
drawCube state object@(ObjectAttributes scaleSize paint location noseVector upVector ambience4 diffuse4 specular4 emission4 shininess) = do

  let w     = 1.0
  
  cubeDisplayList <- (cube w)

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do

      case shininess of 
        (Just sh) -> do 
          materialShininess FrontAndBack $= (iToGL sh)
        _ -> postRedisplay Nothing

      case specular4 of 
        (Just point4) -> do 
          materialSpecular FrontAndBack $= pointToColor4f point4
        _ -> postRedisplay Nothing

      case emission4 of 
        (Just point4) -> do 
          materialEmission FrontAndBack $= pointToColor4f point4
        _ -> postRedisplay Nothing

      case (paint, location, scaleSize) of
        ((Just (Point4 px py pz pa)), (Just (lx, ly, lz)), (Just s)) -> do 
          color3f px py pz
          translate $ vector3f lx ly lz
          scale3f s s s
          
          renderPrimitive Quads $ do
            cube w


  --cubeDisplayList <- (cube 0.25)
  --preservingMatrix $ do
  --  preservingAttrib [AllServerAttributes] $ do
  --    translate $ vector3f x y z
  --    scale3f s s s
  --    callList cubeDisplayList
      postRedisplay Nothing


cube :: Float -> IO ()
cube w = do

  -- Front
  drawNormal3f 0 0 w
  drawVertex3f (-w) (-w)  w
  drawVertex3f w (-w)  w
  drawVertex3f w w  w
  drawVertex3f (-w) w  w
  -- Back
  drawNormal3f 0  0 (-w)
  drawVertex3f w (-w) (-w)
  drawVertex3f (-w) (-w) (-w)
  drawVertex3f (-w) w (-w)
  drawVertex3f w w (-w)
  -- Right
  drawNormal3f w  0  0
  drawVertex3f w (-w) w
  drawVertex3f w (-w) (-w)
  drawVertex3f w w (-w)
  drawVertex3f w w w
  -- Left
  drawNormal3f (-w)  0  0
  drawVertex3f (-w) (-w) (-w)
  drawVertex3f (-w) (-w) w
  drawVertex3f (-w) w w
  drawVertex3f (-w) w (-w)
  -- Top
  drawNormal3f 0 w  0
  drawVertex3f (-w) w w
  drawVertex3f w w w
  drawVertex3f w w (-w)
  drawVertex3f (-w) w (-w)
  -- Bottom
  drawNormal3f 0 (-1) 0
  drawVertex3f (-w) (-w) (-w)
  drawVertex3f w (-w) (-w)
  drawVertex3f w (-w) w
  drawVertex3f (-w) (-w) w

  --cubeDisplayList <- defineNewList Compile $ do
    --renderPrimitive Quads $ do

      --color3f (105/255) (105/255) (105/255)
  --drawNormal3f 
  --vertex $ Vertex3 w w w
  --vertex $ Vertex3 w w (-w)
  --vertex $ Vertex3 w (-w) (-w)
  --vertex $ Vertex3 w (-w) w

  --vertex $ Vertex3 w w w
  --vertex $ Vertex3 w w (-w)
  --vertex $ Vertex3 (-w) w (-w)
  --vertex $ Vertex3 (-w) w w
  
  --vertex $ Vertex3 w w w
  --vertex $ Vertex3 w (-w) w
  --vertex $ Vertex3 (-w) (-w) w
  --vertex $ Vertex3 (-w) w w
  
  --vertex $ Vertex3 (-w) w w
  --vertex $ Vertex3 (-w) w (-w)
  --vertex $ Vertex3 (-w) (-w) (-w)
  --vertex $ Vertex3 (-w) (-w) w
  
  --vertex $ Vertex3 w (-w) w
  --vertex $ Vertex3 w (-w) (-w)
  --vertex $ Vertex3 (-w) (-w) (-w)
  --vertex $ Vertex3 (-w) (-w) w
  
  --vertex $ Vertex3 w w (-w)
  --vertex $ Vertex3 w (-w) (-w)
  --vertex $ Vertex3 (-w) (-w) (-w)
  --vertex $ Vertex3 (-w) w (-w)

  --return cubeDisplayList




--cube :: GLfloat -> IO DisplayList
--cube w = do

--  cubeDisplayList <- defineNewList Compile $ do
--    renderPrimitive Quads $ do

--      color3f (105/255) (105/255) (105/255)
--      vertex $ Vertex3 w w w
--      vertex $ Vertex3 w w (-w)
--      vertex $ Vertex3 w (-w) (-w)
--      vertex $ Vertex3 w (-w) w

--      vertex $ Vertex3 w w w
--      vertex $ Vertex3 w w (-w)
--      vertex $ Vertex3 (-w) w (-w)
--      vertex $ Vertex3 (-w) w w
      
--      vertex $ Vertex3 w w w
--      vertex $ Vertex3 w (-w) w
--      vertex $ Vertex3 (-w) (-w) w
--      vertex $ Vertex3 (-w) w w
      
--      vertex $ Vertex3 (-w) w w
--      vertex $ Vertex3 (-w) w (-w)
--      vertex $ Vertex3 (-w) (-w) (-w)
--      vertex $ Vertex3 (-w) (-w) w
      
--      vertex $ Vertex3 w (-w) w
--      vertex $ Vertex3 w (-w) (-w)
--      vertex $ Vertex3 (-w) (-w) (-w)
--      vertex $ Vertex3 (-w) (-w) w
      
--      vertex $ Vertex3 w w (-w)
--      vertex $ Vertex3 w (-w) (-w)
--      vertex $ Vertex3 (-w) (-w) (-w)
--      vertex $ Vertex3 (-w) w (-w)

--  return cubeDisplayList