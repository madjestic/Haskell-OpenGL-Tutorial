-- This is an attempt to create a modern, consize OpenGL template in 
-- haskell.  It's mainly meant for experimenting with OpenGL.
--   
-- This works well with OpenGL 4.4.0 NVIDIA 361.18
-- Lenovo E431 (2014) laptop, running nVidia GeForce GT 740M/PCIe/SSE2

module Main where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.GLUtil (setUniform)
import Graphics.UI.GLUT.Debugging (reportErrors)
import Control.Monad (forever)
import System.Exit (exitSuccess)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (plusPtr, nullPtr, Ptr)
import Foreign.Storable (sizeOf)
import LoadShaders
data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices
data Projection = Planar                
                deriving Show 
data Shape = Square    Point   Side
           deriving Show     
     
type Drawable   = ([Vertex4 Float],[TexCoord2 Float])
type UV         = [TexCoord2 Float] 
type Point      = (Float, Float)
type Points     = [Point]     
type Side       = Float

square :: Point -> Float -> [Point]
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

toPoints :: Shape -> [Point]
toPoints (Square   pos side)     =  square pos side

toUV :: Projection -> UV
toUV Planar = projectPlanar ps
                  where ps = [(1.0, 1.0),( 0.0, 1.0),( 0.0, 0.0)
                             ,(1.0, 1.0),( 0.0, 0.0),( 1.0, 0.0)]::Points
                                                                   
toDrawable :: Shape -> Drawable
toDrawable x = (vs, uv)
           where
               vs'   = toPoints x               
               uv    = map toTexCoord2 vs'
               vs    = map toVertex4 vs'

toVertex4 :: Point -> Vertex4 Float
toVertex4 (k, l) = Vertex4 k l 0 1

toTexCoord2 :: (a, a) -> TexCoord2 a
toTexCoord2 (k, l) = TexCoord2 k l

projectPlanar :: [Point] -> UV
projectPlanar = map $ uncurry TexCoord2                                                                   

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
resizeWindow win w h =
    do
      GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0   

openWindow :: String -> (Int, Int) -> IO GLFW.Window
openWindow title (sizex,sizey) = do
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
closeWindow win = do
    GLFW.destroyWindow win
    GLFW.terminate

display :: Drawable -> IO ()
display drawable = do
     inWindow <- openWindow "NGL is Not GLoss" (512,512)
     draw inWindow drawable
     closeWindow inWindow
                 
draw :: GLFW.Window -> Drawable -> IO ()
draw win xs = do
    descriptor <- initResources xs
    onDisplay win descriptor                 
    
initResources :: (Drawable) -> IO Descriptor
initResources (vs, uv) = do
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    --
    -- Declaring VBO: vertices
    --
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
        (ToFloat, VertexArrayDescriptor 4 Float 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled
    
    --
    -- Declaring VBO: UVs
    --
    let uv = toUV Planar

    textureBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just textureBuffer
    withArray uv $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head uv))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)
        
    let firstIndex = 0
        uvCoords = AttribLocation 1
    vertexAttribPointer uvCoords $=
        (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset firstIndex))
    vertexAttribArray uvCoords $= Enabled
  
    program <- loadShaders [
        ShaderInfo VertexShader (FileSource "Shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "Shaders/shader.frag")]
    currentProgram $= Just program

-- Set Uniforms
    
    let setUniform var val = do
              location <- get (uniformLocation program var)
              reportErrors
              uniform location $= val
              
    setUniform "fTime" (20.0 :: GLfloat)

    return $ Descriptor triangles firstIndex (fromIntegral numVertices)    

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
               
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

main :: IO ()
main = do
     let drawable = toDrawable $ Square (-0.0, -0.0) 2.0
     display drawable
