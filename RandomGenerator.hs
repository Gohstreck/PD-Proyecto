
module RandomGenerator where
import System.Random
import Numeric.Noise.Perlin
import Puntos
import Graphics.UI.GLUT
import System.IO.Unsafe
{- | La función se encargará de crear una semilla random
para pasarla como parámetro a generatePerlin
-}


generateRandom :: Int
generateRandom =   unsafePerformIO $ do 
  getStdRandom (randomR (1,100))


{- | Esta función se encarga de crear valores aleatorios para y. Recibe dos GLfloat
que nos van a representar las coordenadas (x, s) de un punto y le regresa la coordenada
y a ese punto. Siempre que se manda a llamar recibe una semilla distinta
para que cada que se quiera crear un mapa distinto, las alturas varíen.
-}
generatePerlin :: Int -> GLfloat -> GLfloat ->  GLfloat
generatePerlin s x y =  realToFrac $ noiseValue perlinNoise (realToFrac x, realToFrac y, 3)
    where
      octaves = 1
      scale = 0.5
      persistance = 1.0
      perlinNoise = perlin s octaves scale persistance
