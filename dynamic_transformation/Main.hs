module Main where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad (forever)
import System.Exit (exitSuccess)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (plusPtr, nullPtr, Ptr)
import Foreign.Storable (sizeOf)
import NGL.LoadShaders
import Graphics.GLUtil (readTexture, texture2DWrap)
import Text.Printf

data Descriptor =
     Descriptor VertexArrayObject ArrayIndex NumArrayIndices
data Shape =
  Square    (Float, Float)   Float
           deriving Show

data GLMatrix a =
     GLMatrix !a !a !a !a
              !a !a !a !a
              !a !a !a !a
              !a !a !a !a
                deriving Eq

instance PrintfArg a => Show (GLMatrix a) where
  show (GLMatrix m11 m12 m13 m14
                 m21 m22 m23 m24
                 m31 m32 m33 m34
                 m41 m42 m43 m44) =
    let matrix = "[ %v %v %v %v ]\n\
                 \[ %v %v %v %v ]\n\
                 \[ %v %v %v %v ]\n\
                 \[ %v %v %v %v ]\n"
    in printf matrix m11 m12 m13 m14
                     m21 m22 m23 m24
                     m31 m32 m33 m34
                     m41 m42 m43 m44

verticies :: [GLfloat]
verticies =
  [ -- | positions    -- | colors      -- | uv
    0.5,  0.5, 0.0,   1.0, 0.0, 0.0,   1.0, 1.0,
    0.5, -0.5, 0.0,   0.0, 1.0, 0.0,   1.0, 0.0,
   -0.5, -0.5, 0.0,   0.0, 0.0, 1.0,   0.0, 0.0,
   -0.5,  0.5, 0.0,   0.0, 0.0, 0.0,   0.0, 1.0
  ]

indices :: [GLuint]
indices =
  [          -- Note that we start from 0!
    0, 1, 3, -- First Triangle
    1, 2, 3  -- Second Triangle
  ]
     
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
    inWindow <- openWindow "NGL is Not GLoss" (512,512)
    descriptor <- initResources verticies indices
    onDisplay inWindow descriptor
    closeWindow inWindow
                 
onDisplay :: GLFW.Window -> Descriptor -> IO ()
onDisplay win descriptor@(Descriptor triangles posOffset numVertices) =
  do
    GL.clearColor $= Color4 0 0 0 1
    GL.clear [ColorBuffer]
    bindVertexArrayObject $= Just triangles
    drawElements Triangles 6 GL.UnsignedInt nullPtr
    GLFW.swapBuffers win
   
    forever $ do
       GLFW.pollEvents
       onDisplay win descriptor                 

-- | Init Resources
---------------------------------------------------------------------------
initResources :: [GLfloat] -> [GLuint] -> IO Descriptor
initResources vs idx =
  do
    -- | VAO
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    let numVertices = length verticies
    withArray verticies $ \ptr ->
      do
        let sizev = fromIntegral (numVertices * sizeOf (head verticies))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let numIndices = length indices
    withArray idx $ \ptr ->
      do
        let indicesSize = fromIntegral (numIndices * (length indices))
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)


    -- | Bind the pointer to the vertex attribute data
    let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
        stride     = 8 * floatSize

    -- | Positions
    let vPosition  = AttribLocation 0
        posOffset  = 0 * floatSize
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset posOffset))
    vertexAttribArray vPosition   $= Enabled

    -- | Colors
    let vColor  = AttribLocation 1
        clrOffset  = 3 * floatSize
    vertexAttribPointer vColor $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset clrOffset))
    vertexAttribArray vColor   $= Enabled

    -- | UV
    let uvCoords   = AttribLocation 2
        uvOffset   = 6 * floatSize
    vertexAttribPointer uvCoords  $=
        (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset uvOffset))
    vertexAttribArray uvCoords    $= Enabled


    -- | Assign Textures
    activeTexture            $= TextureUnit 0
    let tex_00 = "Resources/Textures/container.jpg"
    tx0 <- loadTex tex_00
    texture Texture2D        $= Enabled
    textureBinding Texture2D $= Just tx0
    
    activeTexture            $= TextureUnit 1
    let tex_01 = "Resources/Textures/awesomeface.png"
    tx1 <- loadTex tex_01
    textureBinding Texture2D $= Just tx1
    texture Texture2D        $= Enabled
    
    -- || Shaders
    program <- loadShaders [
        ShaderInfo VertexShader   (FileSource "Shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "Shaders/shader.frag")]
    currentProgram $= Just program

    -- Set Uniforms
    location0 <- get (uniformLocation program "tex_00")
    uniform location0 $= (TextureUnit 0)
    location1 <- get (uniformLocation program "tex_01")
    uniform location1 $= (TextureUnit 1)

    -- Set Transform Matrix
    let tr =
          [ 1, 0, 0, 0
          , 0, 1, 0, 0
          , 0, 0, 1, 0
          , 0, 0, 0, 0.5 ] :: [GLfloat]
          
    transform <- GL.newMatrix ColumnMajor tr :: IO (GLmatrix GLfloat)
    location2 <- get (uniformLocation program "transform")
    uniform location2 $= (transform)
    
    -- || Unload buffers
    bindVertexArrayObject         $= Nothing
    -- bindBuffer ElementArrayBuffer $= Nothing

    return $ Descriptor triangles posOffset (fromIntegral numIndices)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

loadTex :: FilePath -> IO TextureObject
loadTex f =
  do
    t <- either error id <$> readTexture f
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texture2DWrap $= (Repeated, ClampToEdge)
    return t
---------------------------------------------------------------------------

main :: IO ()
main =
  do
    display
