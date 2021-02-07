module Matrices2 where
import RandomGenerator
import Puntos
import Graphics.UI.GLUT

nP :: GLfloat -> GLfloat -> GLfloat -> Color3 GLfloat -> Punto3C
nP x y s c = P3 x y s c

incremento :: GLfloat
incremento = 0.1

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

{- | Se encargará de crear una matriz de puntos de 100 x 100 (x, s). Cada punto y
se creará con la función de generate Perlin
-}

colorFromValue :: GLfloat -> Color3 GLfloat
colorFromValue n =
  do
  	let t = (\i -> 0.5 + 0.5*cos (i / 10))
  	Color3 (t n) (t (n + 2)) (t (n + 10))

{- | Genera una matriz de Puntos. 
Primero crea todas las tuplas del estilo (x,s), luego a cada tupla 
le aplica una función para obtener su valor del perlin noise 
de acuerdo a los valores especificados por RandomGenerator y los 
mete dentro del constructor.
-}
generateVertexMatrix :: [Punto3C]
generateVertexMatrix = map creaPuntos $ [(a, b) | a <- xs, b <- xs]
  where
    xs = createLine 100

{- | Genera una matriz de Puntos con sus respectivas normales.

-}
generateVertexNormalMatrix :: [Punto3CN]
generateVertexNormalMatrix = map sacaNormales generateVertexMatrix


{- | Recibe una tupla de GLfloat @(x,s) y regresa un punto de la forma: 
P @x@ (perlinNoise x s) @s@ (colorFromValue y). El color se obtiene de la función
colorFromValue que le otorga colores diferentes a alturas diferentes.
-}
creaPuntos :: (GLfloat, GLfloat) -> Punto3C
creaPuntos (n1,n2) = P3 n1 y n2 (colorFromValue y)
  where y = generatePerlin generateRandom n1 n2

{- | Recibe un punto @p y regresa los puntos que se encuentran en su cuadrícula teniendo
a @p@ como esquina inferior izquierda. 

--------------------
|arriba     diagonal|
|                   |
|p          der     |
|___________________|

-}
agrupa4 :: Punto3C -> [Punto3C]
agrupa4 p = [p, arriba, diagonal,der]
	where
		x1 = (x p) + incremento
		s1 = (s p) + incremento
		y1 = generatePerlin generateRandom x1 (s p)
		y2 = generatePerlin generateRandom (x p) s1
		y3 = generatePerlin generateRandom x1 s1
		der = P3 x1 y1 (s p) (Color3 0 0 0)
		arriba = P3 (x p) y2 s1 (Color3 0 0 0)
		diagonal = P3 x1 y3 s1 (Color3 0 0 0)


{- | Recibe un Punto3CN @p, obtiene sus vecinos y les saca la normal a cada uno
usando la función sacaNormales
-}
agrupa4N :: Punto3CN ->  [Punto3CN]
agrupa4N p = map (sacaNormales) (agrupa4 (P3 (x1 p) (y1 p) (s1 p) (c1 p)))

{- | La normal se calcula obteniendo 2 puntos vecinos (arriba y der), les aplica la resta de vectores
y saca su producto cruz.
-}
sacaNormales :: Punto3C -> Punto3CN
sacaNormales p1 = P3N (x p1) p1xnn (y p1) p1ynn (s p1) p1snn (c p1)
  where
    vecinos = agrupa4 p1
    p2 = vecinos !! 1
    p3 = vecinos !! 3
    u = (P3 ((x p2)-(x p1)) ((y p2)-(y p1)) ((s p2)-(s p1)) (Color3 0 0 0))
    v = (P3 ((x p3)-(x p1)) ((y p3)-(y p1)) ((s p3)-(s p1)) (Color3 0 0 0))
    p1xn = ((y u) * (s v)) - ((s u) * (y v))
    p1yn = ((s u) * (x v)) - ((x u) * (s v))
    p1sn = ((x u) * (y v)) - ((y u) * (x v))
    ss = sqrt (p1xn * p1xn + p1yn * p1yn + p1sn * p1sn) 
    p1xnn = p1xn / ss 
    p1ynn = p1yn / ss
    p1snn = p1sn / ss