import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
  GLFW.init
  GLFW.defaultWindowHints
  Just win <- GLFW.createWindow 640 480 "GLFW Demo" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  onDisplay win
  GLFW.destroyWindow win
  GLFW.terminate

onDisplay :: Window -> IO ()
onDisplay win = do
  GL.clearColor $= Color4 1 0 0 1
  GL.clear [ColorBuffer]
  GLFW.swapBuffers win
  onDisplay win
