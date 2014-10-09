module Cube (drawCube) where 
 
import Graphics.UI.GLUT

import GLUtils


drawCube :: Float -> (Float, Float, Float) -> IO ()
drawCube s (x, y, z) = do

  let w = 0.25
      white = (Point4 1 1 1 1)
      black = (Point4 0 0 0 1)
  postRedisplay Nothing

  --preservingMatrix $ do
  --  preservingAttrib [AllServerAttributes] $ do

  --    renderPrimitive Quads $ do
  --      color3f (105/255) (105/255) (105/255)
  --      translate $ vector3f x y z
  --      scale3f s s s

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


  --cubeDisplayList <- (cube 0.25)
  --preservingMatrix $ do
  --  preservingAttrib [AllServerAttributes] $ do
  --    translate $ vector3f x y z
  --    scale3f s s s
  --    callList cubeDisplayList
 
cube :: GLfloat -> IO DisplayList
cube w = do

  cubeDisplayList <- defineNewList Compile $ do
    renderPrimitive Quads $ do

      color3f (105/255) (105/255) (105/255)
      vertex $ Vertex3 w w w
      vertex $ Vertex3 w w (-w)
      vertex $ Vertex3 w (-w) (-w)
      vertex $ Vertex3 w (-w) w

      vertex $ Vertex3 w w w
      vertex $ Vertex3 w w (-w)
      vertex $ Vertex3 (-w) w (-w)
      vertex $ Vertex3 (-w) w w
      
      vertex $ Vertex3 w w w
      vertex $ Vertex3 w (-w) w
      vertex $ Vertex3 (-w) (-w) w
      vertex $ Vertex3 (-w) w w
      
      vertex $ Vertex3 (-w) w w
      vertex $ Vertex3 (-w) w (-w)
      vertex $ Vertex3 (-w) (-w) (-w)
      vertex $ Vertex3 (-w) (-w) w
      
      vertex $ Vertex3 w (-w) w
      vertex $ Vertex3 w (-w) (-w)
      vertex $ Vertex3 (-w) (-w) (-w)
      vertex $ Vertex3 (-w) (-w) w
      
      vertex $ Vertex3 w w (-w)
      vertex $ Vertex3 w (-w) (-w)
      vertex $ Vertex3 (-w) (-w) (-w)
      vertex $ Vertex3 (-w) w (-w)

  return cubeDisplayList