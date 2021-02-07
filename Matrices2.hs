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

{- | Se encargará de crear una matris de puntos de 100 x 100 (x, s). Cada punto y
se creará con la función de generate Perlin
-}

colorFromValue :: GLfloat -> Color3 GLfloat
colorFromValue n =
  do
  	let t = (\i -> 0.5 + 0.5*cos (i / 10))
  	Color3 (t n) (t (n + 2)) (t (n + 10))

generateVertexMatrix :: [Punto3C]
generateVertexMatrix = map creaPuntos $ [(a, b) | a <- xs, b <- xs]
  where
    xs = createLine 100

generateVertexNormalMatrix :: [Punto3CN]
generateVertexNormalMatrix = map sacaNormales generateVertexMatrix

creaPuntos :: (GLfloat, GLfloat) -> Punto3C
creaPuntos (n1,n2) = P3 n1 y n2 (colorFromValue y)
  where y = generatePerlin generateRandom n1 n2


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

agrupa4N :: Punto3CN ->  [Punto3CN]
agrupa4N p = map (sacaNormales) (agrupa4 (P3 (x1 p) (y1 p) (s1 p) (c1 p)))
{-	where
		derX = (x1 p) + incremento
		arribaS = (s1 p) + incremento
		derY = generatePerlin generateRandom derX (s1 p)
		arribaY = generatePerlin generateRandom (x1 p) arribaS
		diagonalY = generatePerlin generateRandom derX arribaS
		der = P3 x2 y2 (s p) (Color3 0 0 0)
		abajo = P3 (x1 p) y3 s2 (Color3 0 0 0)
		diagonal = P3 x1 y3 s2 (Color3 0 0 0)
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