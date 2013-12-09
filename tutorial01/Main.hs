import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad


resizeWindow :: Size -> IO ()
resizeWindow =
  \ size@(GL.Size w h) ->
    do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

  
main :: IO ()
main = do
  GLFW.initialize
  GLFW.openWindow (GL.Size 640 480) [] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"
  GLFW.windowSizeCallback $= resizeWindow
  onDisplay
  GLFW.closeWindow
  GLFW.terminate


onDisplay :: IO ()
onDisplay = do
  GL.clearColor $= Color4 1 0 0 1
  GL.clear [ColorBuffer]
  GLFW.swapBuffers

  p <- GLFW.getKey GLFW.ESC
  unless (p == GLFW.Press) $ 
    onDisplay

