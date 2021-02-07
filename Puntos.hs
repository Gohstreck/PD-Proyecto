module Puntos where
import Graphics.UI.GLUT
data Punto3C = P3 {x :: GLfloat, y :: GLfloat, s :: GLfloat, c :: Color3 GLfloat} deriving Eq
data Punto3CN = P3N {
x1 :: GLfloat,
xn :: GLfloat,
y1 :: GLfloat,
yn :: GLfloat,
s1 :: GLfloat,
sn :: GLfloat,
c1 :: Color3 GLfloat} deriving Eq

data Obj = O{
  vertices :: [Punto3C],
  caras :: [[Punto3C]]
}

instance Show Punto3C where
    show p = "v " ++ show (x p) ++ ", " ++ show (y p) ++ ", " ++ show (s p) ++ "\n"

instance Show Punto3CN where
	show p = "v " ++ show (x1 p) ++ ", " ++ show (y1 p) ++ ", " ++ show (s1 p) ++ "\nvn " ++ show (xn p) ++ ", " ++ show (yn p) ++ ", " ++ show (sn p) ++ "\n"
