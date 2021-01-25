module CreateMatrix where
import Numeric.Noise.Perlin
import Numeric.Decimal
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Random

{- | Crea una lista infinita desde el 1 aumentando el valor de cada elemento
en 0.01 cada ves que se vuelve a mandar a llamar.
-}
createLineAux :: [GLfloat]
createLineAux = map (\x -> x*0.01 :: GLfloat) [1..]

{- | La función recibe un entero @x y se encarga de crear una línea de tamaño 
@x@ con valores desde el 1, aumentando de 0.01 en 0.01
-}
createLine :: Int -> [GLfloat]
createLine x = take x createLineAux 

{- | La función se encargará de crear una semilla random 
para pasarla como parámetro a generatePerlin
-}
generateRandom :: IO Int
generateRandom = do
  r1 <- getStdGen
--  return (round $ (take 1 ns) * 100 )
  let (x, r2) = randomR (0,100) r1
  setStdGen r2
  return x

{- | Esta función se encarga de crear valores aleatorios para y. Recibe dos GLfloat
que nos van a representar las coordenadas (x, s) de un punto y le regresa la coordenada
y a ese punto. Siempre que se manda a llamar recibe una semilla distinta
para que cada que se quiera crear un mapa distinto, las alturas varíen.
-}
generatePerlin :: IO Int -> GLfloat -> GLfloat ->  IO GLfloat
generatePerlin s x y = do 
    let octaves     = 5
    let scale       = 0.05
    let persistance = 0.5
    s2              <- s
    let perlinNoise = perlin s2 octaves scale persistance 
    return $ realToFrac $ noiseValue perlinNoise (realToFrac x, realToFrac y, 3)
{- | Se encargará de crear una matris de puntos de 100 x 100 (x, s). Cada punto y
se creará con la función de generate Perlin
-}
generateVertexMatrixAux :: IO Int -> [(GLfloat, IO GLfloat, GLfloat)]
generateVertexMatrixAux r = do 
  let xs = createLine 100
  let ss = createLine 100
  map (\ (x, s) -> (x, (generatePerlin r x s ), s) ) $ zipWith (,) xs ss
 -- let res = map (\(x, y, s) -> (x, y, s, colorFromValue y))

generateMatrix :: [(GLfloat, IO GLfloat, GLfloat)]
generateMatrix = generateVertexMatrixAux generateRandom

{-
colorFromValue :: IO GLfloat -> Color3 GLfloat
colorFromValue n =
  do 
    n2 <- n
    let t = (\i -> 0.5 + 0.5*cos (fromIntegral i / 10))

    Color3 (t n2) (t (n2 + 2)) (t (n2 + 10))
-}