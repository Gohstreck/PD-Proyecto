module Main where
import Graphics.UI.GLUT
import Matrices2
import Puntos

{- Falta:
explicar por qué mapM_
Importar elementos 3d
-}

-- Cuánto nos moveremos al usar las teclas wasd
movimiento :: GLfloat
movimiento = 0.25

-- El ángulo que nos moveremos usando las teclas ijkl
rotacion :: GLfloat
rotacion = 1



main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize  --parámetros que necesita GLUT para funcionar
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, RGBMode] 
  createWindow "Escenario 3d"
  depthFunc       $= Just Less

{-
  blend           $= Enabled
  blendFunc       $= (SrcAlpha, OneMinusSrcAlpha)-}

  -- Le decimos dónde tiene que aplicar color a los materiales y qué luces le afectan
  colorMaterial   $= Just (FrontAndBack, AmbientAndDiffuse)
  reshapeCallback $= Just reshape 
  clearColor      $= Color4 0.0 0.0 0.0 (1.0 :: GLfloat)

  -- Ponemos la iluminación
  lighting        $= Enabled
  light (Light 0) $= Enabled
  shadeModel $= Smooth

  -- Ponemos las luces
  lightModelAmbient $= Color4 0.57 0.31 0.05 1
  diffuse (Light 0) $= Color4 0.74 0.65 0.05 1
  specular (Light 0) $= Color4 1 1 1 1
  position (Light 0) $= Vertex4 (5) 20 (5) 1

  --Ponemos los valores necesarios para la interacción entre los materiales y la luz
  
  materialAmbient  FrontAndBack $= (Color4 0.02 0.32 0.23 (1.0 :: GLfloat))
  materialDiffuse FrontAndBack $= (Color4 0.29 0.6142 0.3 (1.0 :: GLfloat))
  materialSpecular  FrontAndBack  $= (Color4 1 1 1 (1 :: GLfloat))
  materialShininess FrontAndBack $= 5 

  -- La matriz con la que trabajaremos
  matrixMode $= Modelview 0

  -- La función que se encargará de dibujar la escena
  displayCallback $=  display 

  -- La función que se encargará de la interacción con el teclado.
  keyboardCallback $= Just keyboard

  -- Iniciamos el loop de OpenGL
  mainLoop


display :: DisplayCallback
display= do
  clear [ColorBuffer, DepthBuffer] -- Limpiamos los buffers (la pantalla)
  preservingMatrix $ drawMatrix    -- Pedimos que se mantenga la matriz
  swapBuffers                      -- Cambiamos el buffer entre los dos que dibujamos

{- | Esta función está dibujando rectángulos, cada 4 puntos dibujados los une como 
si fuera un rectángulo.
-}
drawMatrix :: IO ()
drawMatrix = do
  renderPrimitive Quads $ do
      mapM_  drawPointsNormal generateVertexNormalMatrix

{- | Recibe un punto p, y dibuja no solo a p, sino a sus vecinos de arriba,
diagonal y derecha (en ese orden) para que el cuadrado cierre teniéndolo a él
de nuevo al inicio.
-}
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

{- | Esta función se manda a llamar cada vez que se modifica el tamaño de la ventana
y también antes de crear la primer ventana.
viewPort es "la ventana" que tenemos hacia el mundo que se está dibujando. Puede 
que la ventana del programa mida 1000 x 1000 pixeles, pero solo ocupemos un viewport
de 100 x 100, dejando a los otros 900 x 900 pixeles en negro.
Primero mandamos a llamar a la matriz de proyección porque vamos a ajustar la proyección.
Cargamos la identidad (para que no se vayan sobreescribiendo las vistas cada 
que cambiemos el tamaño).
Aplicamos perspective para ver 60°, con una relación x / y de 1, teniendo como 
primer plano -10 y como segundo 10.
Iniciamos la matriz de ModelView
Cargamos la identidad.
Colocamos la cámara.

-}
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

{- | Aquí no estoy seguro de qué signifique Position.
Los char simbolizan las teclas que se pulsen.
Como modoficar lookAt significaría tener que tener variables
para la posición de la cámara, y como la cámara al final
solo es lo mismo que hacer translaciones y rotaciones 
(por eso la cámara se pone dentro de ModelView) decidí por mejor
hacer las rotaciones y traslaciones a mano.
Ya que están hechas las transformaciones, mandamos a llamar a display
con los nuevos valores
-}
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
