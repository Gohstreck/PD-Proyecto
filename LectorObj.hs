module LectorObj where
import Puntos
import Graphics.UI.GLUT
import Data.List
import Data.List.Split
import System.IO
baseDatos = "cactus.obj"

creaPuntos :: IO [Punto3C]
creaPuntos =
  do
    contenido <- readFile baseDatos
    creaObj $ splitOn "\n" contenido

creaObj :: [String] -> creaObj
creaObj [] =
creaObj (x:xs)
