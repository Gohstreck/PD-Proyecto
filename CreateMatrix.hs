module ManipulaMatrices where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import RandomGenerator


--import Test.QuickCheck.Random 
type Punto = (GLfloat, GLfloat, GLfloat)


incremento :: GLfloat
incremento = 0.0001

{- | Crea una lista infinita desde el 1 aumentando el valor de cada elemento
en 0.01 cada ves que se vuelve a mandar a llamar.
-}

createLineAux :: [GLfloat]
createLineAux = map (*incremento) [0..]

{- | La función recibe un entero @x y se encarga de crear una línea de tamaño 
@x@ con valores desde el 1, aumentando de 0.01 en 0.01
-}
createLine :: Int -> [GLfloat]
createLine x = take x createLineAux 

{- | Se encargará de crear una matris de puntos de 100 x 100 (x, s). Cada punto y
se creará con la función de generate Perlin
-}
generateVertexMatrixAux :: Int -> [(GLfloat, GLfloat, GLfloat)]
generateVertexMatrixAux r = 
  map (\ (x, s) -> (x, (generatePerlin r x s ), s) ) $ [(a, b) | a <- xs, b <- xs]
  where 
    xs = createLine 100

generateMatrix :: [(GLfloat, GLfloat, GLfloat)]
generateMatrix = generateVertexMatrixAux generateRandom



imprimeMatrix :: IO ()
imprimeMatrix = do
  mapM_ (imprime)  generateMatrix

imprime :: (IO GLfloat, IO GLfloat, IO GLfloat) -> IO ()
imprime (a, b, c) = do
  a2 <- a
  b2 <- b
  c2 <- c 
  putStrLn $ "x: "++(show a2) ++ "   y: " ++ (show b2) ++ "    s: "++(show c2)
{-
colorFromValue :: IO GLfloat -> Color3 GLfloat
colorFromValue n =
  do 
    n2 <- n
    let t = (\i -> 0.5 + 0.5*cos (fromIntegral i / 10))

    Color3 (t n2) (t (n2 + 2)) (t (n2 + 10))
-}