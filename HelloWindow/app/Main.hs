module Main where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad (forever)
import System.Exit (exitSuccess)

keyPressed :: GLFW.KeyCallback 
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()
                                                                  
shutdown :: GLFW.WindowCloseCallback
shutdown win =
  do
    GLFW.destroyWindow win
    GLFW.terminate
    _ <- exitSuccess
    return ()                                                                  
     
resizeWindow :: GLFW.WindowSizeCallback
resizeWindow _ w h =
  do
    GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 (realToFrac w) (realToFrac h) 0   

openWindow :: String -> (Int, Int) -> IO GLFW.Window
openWindow title (sizex,sizey) =
  do
    GLFW.init
    GLFW.defaultWindowHints
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 5)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    Just win <- GLFW.createWindow sizex sizey title Nothing Nothing
    GLFW.makeContextCurrent (Just win)
    GLFW.setWindowSizeCallback win (Just resizeWindow)
    GLFW.setKeyCallback win (Just keyPressed)
    GLFW.setWindowCloseCallback win (Just shutdown)
    return win

closeWindow :: GLFW.Window -> IO ()
closeWindow win =
  do
    GLFW.destroyWindow win
    GLFW.terminate

display :: IO ()
display =
  do
    inWindow <- openWindow "Hello Window!" (512,512)
    onDisplay inWindow
    closeWindow inWindow
                 
onDisplay :: GLFW.Window -> IO ()
onDisplay win =
  do
    GL.clearColor $= Color4 1 0 0 1
    GL.clear [ColorBuffer]
    GLFW.swapBuffers win
   
    forever $ do
       GLFW.pollEvents
       onDisplay win

---------------------------------------------------------------------------

main :: IO ()
main =
  do
    display
