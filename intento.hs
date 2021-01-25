import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Random
import GHC.Float
import CreateMatrix
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World" 
  displayCallback $=  display
  reshapeCallback $= Just reshape
  pointSize       $= 3


  mainLoop
 
display :: DisplayCallback
display = do
  clear [ColorBuffer]
  loadIdentity
  preservingMatrix drawMatrix    
  flush

drawMatrix :: IO a
drawMatrix = do
  renderPrimitive Points $ do
  
  mapM_ drawPoints generateMatrix

  where
    drawPoints (a,b,c) = do
      b2 <- b
      color Color3 1 1 (1 :: GLfloat)
      vertex $ Vertex3 a b c

reshape :: ReshapeCallback
reshape s = do
  viewport $= (Position 0 0, s)
  postRedisplay Nothing

