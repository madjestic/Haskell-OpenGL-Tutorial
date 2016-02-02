-- This is a bare-bones version OpenGL template in haskell. 
--   
-- This works well with OpenGL 4.4.0 NVIDIA 361.18
-- Lenovo E431 (2014) laptop, running nVidia GeForce GT 740M/PCIe/SSE2

module Main where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad (forever)
import System.Exit (exitSuccess)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (plusPtr, nullPtr, Ptr)
import Foreign.Storable (sizeOf)
import NGL.LoadShaders

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices
data Shape = Square    (Float, Float)   Float
           deriving Show     
     
square :: (Float, Float) -> Float -> [(Float, Float)]
square pos side = [p1, p2, p3,
                   p1, p3, p4]
    where          
        x = fst pos
        y = snd pos
        r = side/2 
        p1 = (x + r, y + r)
        p2 = (x - r, y + r)
        p3 = (x - r, y - r)
        p4 = (x + r, y - r)

toPoints :: Shape -> [(Float, Float)]
toPoints (Square pos side)     =  square pos side

toVertex4 :: (Float, Float) -> Vertex2 Float
toVertex4 (k,l) = Vertex2 k l

keyPressed :: GLFW.KeyCallback 
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()
                                                                  
shutdown :: GLFW.WindowCloseCallback
shutdown win = do
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
openWindow title (sizex,sizey) = do
    GLFW.init
    GLFW.defaultWindowHints
    Just win <- GLFW.createWindow sizex sizey title Nothing Nothing
    GLFW.makeContextCurrent (Just win)
    GLFW.setWindowSizeCallback win (Just resizeWindow)
    GLFW.setKeyCallback win (Just keyPressed)
    GLFW.setWindowCloseCallback win (Just shutdown)
    return win

closeWindow :: GLFW.Window -> IO ()
closeWindow win = do
    GLFW.destroyWindow win
    GLFW.terminate

display :: [Vertex2 Float] -> IO ()
display xs = do
     inWindow <- openWindow "NGL is Not GLoss" (512,512)
     descriptor <- initResources xs
     onDisplay inWindow descriptor
     closeWindow inWindow
                 
onDisplay :: GLFW.Window -> Descriptor -> IO ()
onDisplay win descriptor@(Descriptor triangles firstIndex numVertices) = do
  GL.clearColor $= Color4 0 0 0 1
  GL.clear [ColorBuffer]
  bindVertexArrayObject $= Just triangles
  drawArrays Triangles firstIndex numVertices
  GLFW.swapBuffers win

  forever $ do
     GLFW.pollEvents
     onDisplay win descriptor                 

initResources :: [Vertex2 Float] -> IO Descriptor
initResources vs = do
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    let vertices = vs
        numVertices = length vertices

    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    withArray vs $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    let firstIndex = 0
        vPosition = AttribLocation 0
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled

    program <- loadShaders [
        ShaderInfo VertexShader (FileSource "Shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "Shaders/shader.frag")]
    currentProgram $= Just program

    return $ Descriptor triangles firstIndex (fromIntegral numVertices)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

main :: IO ()
main = do
     let drawable = map toVertex4 $ toPoints $ Square (-0.0, -0.0) 1.0
     display drawable
