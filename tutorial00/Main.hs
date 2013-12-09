import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
  GLFW.initialize
  GLFW.openWindow (GL.Size 640 480) [] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"
  onDisplay
  GLFW.closeWindow
  GLFW.terminate


onDisplay :: IO ()
onDisplay = do
  GL.clearColor $= Color4 1 0 0 1
  GL.clear [ColorBuffer]
  GLFW.swapBuffers
  onDisplay

