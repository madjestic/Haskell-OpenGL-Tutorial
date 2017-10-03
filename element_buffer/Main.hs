module Main where

import Graphics.Rendering.OpenGL as GL
import Graphics.GL.Core33
import Graphics.GL.Types
import Graphics.UI.GLFW as GLFW
import Control.Monad (forever)
import System.Exit (exitSuccess)
import Foreign.Marshal.Array (withArray, newArray)
import Foreign.Marshal.Alloc  
import Foreign.Ptr (castPtr, plusPtr, nullPtr, Ptr)
import Foreign.Storable (sizeOf, peek)
import NGL.LoadShaders

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices
data Shape = Square    (Float, Float)   Float
           deriving Show

verticies :: [GLfloat]
verticies =
  [
    0.5,  0.5, 0.0,  -- Top Right
    0.5, -0.5, 0.0,  -- Bottom Right
   -0.5, -0.5, 0.0,  -- Bottom Left
   -0.5,  0.5, 0.0   -- Top Left    
  ] 

indices :: [GLuint]
indices =
  [          -- Note that we start from 0!
    0, 1, 3, -- First Triangle
    1, 2, 3  -- Second Triangle
  ]
     
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
    -- GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    -- GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    -- GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    -- GLFW.windowHint (GLFW.WindowHint'Resizable False)
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

display :: IO ()
display = do
     inWindow <- openWindow "NGL is Not GLoss" (512,512)
     descriptor <- initResources verticies indices
     onDisplay inWindow descriptor
     closeWindow inWindow
                 
onDisplay :: GLFW.Window -> Descriptor -> IO ()
onDisplay win descriptor@(Descriptor triangles firstIndex numVertices) = do
  GL.clearColor $= Color4 0 0 0 1
  GL.clear [ColorBuffer]
  bindVertexArrayObject $= Just triangles
  --drawArrays Triangles firstIndex numVertices
  --drawArrays TriangleFan firstIndex numVertices
  drawElements Triangles 6 GL.UnsignedInt nullPtr
  GLFW.swapBuffers win

  forever $ do
     GLFW.pollEvents
     onDisplay win descriptor                 

initResources :: [GLfloat] -> [GLuint] -> IO Descriptor
initResources vs idx =
  do
          -- setup our verticies
    let verticies = [
            0.5,  0.5, 0.0,  -- Top Right
            0.5, -0.5, 0.0,  -- Bottom Right
            -0.5, -0.5, 0.0, -- Bottom Left
            -0.5,  0.5, 0.0  -- Top Left
            ] :: [GLfloat]
    let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length verticies)
    verticesP <- newArray verticies

    -- setup the indexes
    let indices = [  -- Note that we start from 0!
            0, 1, 3, -- First Triangle
            1, 2, 3  -- Second Triangle
            ] :: [GLuint]
    let indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * (length indices)
    indicesP <- newArray indices

    -- -- setup a vertex array object
    -- vaoP <- malloc
    -- glGenVertexArrays 1 vaoP
    -- vao <- peek vaoP
    -- glBindVertexArray vao

    -- -- setup a vertex buffer object and send it data
    -- vboP <- malloc
    -- glGenBuffers 1 vboP
    -- vbo <- peek vboP
    -- glBindBuffer GL_ARRAY_BUFFER vbo
    -- glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesP) GL_STATIC_DRAW

    -- -- setup an element buffer object and send it data
    -- eboP <- malloc
    -- glGenBuffers 1 eboP
    -- ebo <- peek eboP
    -- glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
    -- glBufferData GL_ELEMENT_ARRAY_BUFFER indicesSize (castPtr indicesP) GL_STATIC_DRAW

    -- -- assign the attribute pointer information
    -- let threeFloats = fromIntegral $ sizeOf (0.0::GLfloat) * 3
    -- glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE threeFloats nullPtr
    -- glEnableVertexAttribArray 0

    -- -- unbind our vertex array object to prevent accidental changes in
    -- -- between our draw calls.
    -- glBindVertexArray 0

    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    let vs = verticies
    let numVertices = length vs
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    withArray vs $ \ptr -> do
        let sizev = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    theVao <- genObjectName
    bindVertexArrayObject $= Just theVao

    let firstIndex = 0
        vPosition = AttribLocation 0
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled

    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let numIndices = length indices
    withArray idx $ \ptr -> do
        let sizei = fromIntegral (numIndices * sizeOf (head indices))
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)
    
    program <- loadShaders [
        ShaderInfo VertexShader (FileSource "Shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "Shaders/shader.frag")]
    currentProgram $= Just program

    bindVertexArrayObject $= Nothing
    --bindBuffer ElementArrayBuffer $= Nothing
    
    return $ Descriptor theVao firstIndex (fromIntegral numIndices)
    --return $ Descriptor triangles firstIndex (fromIntegral 21)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

main :: IO ()
main =
  do
     -- let drawable = map toVertex4 $ toPoints $ Square (-0.0, -0.0) 1.0
     -- display drawable
    display
