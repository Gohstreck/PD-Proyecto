module Main where
import Graphics.UI.GLUT
import Matrices2
import Puntos

{- Falta:
Dibujar por 4
Cambiar la posición de la cámara
Cambiar la pos de la lus
Importar elementos 3d
-}

movimiento :: GLfloat
movimiento = 0.25

rotacion :: GLfloat
rotacion = 1

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, RGBMode]
  createWindow "Escenario 3d"
  depthFunc       $= Just Less


  blend           $= Enabled
  blendFunc       $= (SrcAlpha, OneMinusSrcAlpha)
  colorMaterial   $= Just (FrontAndBack, AmbientAndDiffuse)
  reshapeCallback $= Just reshape 
  clearColor      $= Color4 0.0 0.0 0.0 (1.0 :: GLfloat)

  lighting        $= Enabled
  light (Light 0) $= Enabled
  shadeModel $= Smooth

  lightModelAmbient $= Color4 0.57 0.31 0.05 1
  diffuse (Light 0) $= Color4 0.74 0.65 0.05 1
  specular (Light 0) $= Color4 0.90 0.78 0.06 1
  position (Light 0) $= Vertex4 (5) 20 (5) 1

  materialAmbient  FrontAndBack $= (Color4 0.02 0.32 0.23 (1.0 :: GLfloat))
  materialDiffuse FrontAndBack $= (Color4 0.29 0.6142 0.3 (1.0 :: GLfloat))
  materialSpecular  FrontAndBack  $= (Color4 1 1 1 (1 :: GLfloat))
  materialShininess FrontAndBack $= 5 

  matrixMode $= Modelview 0

  displayCallback $=  display 
  keyboardCallback $= Just keyboard
  mainLoop

display :: DisplayCallback
display= do
  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  preservingMatrix $ drawMatrix
  swapBuffers

drawMatrix :: IO ()
drawMatrix = do
  renderPrimitive Quads $ do
      mapM_  drawPointsNormal generateVertexNormalMatrix


drawPointsNormal :: Punto3CN -> IO ()
drawPointsNormal p = do
    color $ c1 p
    normal $ Normal3 ( xn p) ( yn p) (sn p)
    vertex $ Vertex3 (x1 p) (y1 p) (s1 p)

    normal $ Normal3 (xn arriba) (yn arriba) (sn arriba)
    vertex $ Vertex3 (x1 arriba) (y1 arriba) (s1 arriba)

    normal $ Normal3 (xn diagonal) (yn diagonal) (sn diagonal)
    vertex $ Vertex3 (x1 diagonal) (y1 diagonal) (s1 diagonal)

    normal $ Normal3 (xn der) (yn der) (sn der)
    vertex $ Vertex3 (x1 der) (y1 der) (s1 der)

    where
      vecindad = agrupa4N p
      arriba = (vecindad !! 1)
      diagonal = (vecindad !! 2)
      der  = (vecindad !! 3)

reshape :: ReshapeCallback
reshape (Size x 0) = reshape (Size x 1)
reshape s@(Size x y) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 60 1 (-10) (10 :: GLdouble)
  matrixMode $= Modelview 0
  loadIdentity
  lookAt (Vertex3 5 10 15) (Vertex3 5 1 5) (Vector3 0 1 (0 ::GLdouble))
  postRedisplay Nothing
  swapBuffers

keyboard :: Char -> Position ->  IO ()
keyboard 'w' a = do
  matrixMode $= Modelview 0
  translate $ Vector3 0 0 (movimiento)
  display
keyboard 's' _ = do
  matrixMode $= Modelview 0
  translate $ Vector3 0 0 (-movimiento)
  display
keyboard 'a' _ = do 
  matrixMode $= Modelview 0
  translate $ Vector3 (movimiento) 0 0
  display
keyboard 'd' _ = do
  matrixMode $= Modelview 0 
  translate $ Vector3 (-movimiento) 0 0
  display
keyboard 'i' a = do
  matrixMode $= Modelview 0
  rotate rotacion (Vector3 (1) 0 0)
  display
keyboard 'k' _ = do
  matrixMode $= Modelview 0
  rotate rotacion (Vector3 (-1) 0 0)
  display
keyboard 'j' _ = do 
  matrixMode $= Modelview 0
  rotate rotacion (Vector3 0 0 1)
  display
keyboard 'l' _ = do
  matrixMode $= Modelview 0 
  rotate rotacion (Vector3 0 0 (-1))
  display
keyboard '\ESC' _ = do leaveMainLoop
keyboard ' ' _ = do 
  matrixMode $= Modelview 0
  scale (1.1) (1.1) (1.1 :: GLfloat)
  display
keyboard x _ = do
      putStrLn "Las teclas válidas para moverse son: "
      putStrLn "W A S D para mover la posición de la cámara"
      putStrLn "I J K L para rotar la cámara"
