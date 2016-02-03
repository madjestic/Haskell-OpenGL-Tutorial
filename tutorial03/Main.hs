import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.GLUtil (readTexture, texture2DWrap)
import Control.Monad (forever)
import System.Exit ( exitWith, ExitCode(..) )
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (plusPtr, nullPtr, Ptr)
import Foreign.Storable (sizeOf)
import LoadShaders


data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices


bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral


initResources :: IO Descriptor
initResources = do
  triangles <- genObjectName
  bindVertexArrayObject $= Just triangles

  let vs  = [Vertex2 (-0.90) (-0.90)  -- Triangle 1
            ,Vertex2   0.85  (-0.90)
            ,Vertex2 (-0.90)   0.85 
            ,Vertex2   0.90  (-0.85)  -- Triangle 2
            ,Vertex2   0.90    0.90 
            ,Vertex2 (-0.85)   0.90] :: [Vertex2 GLfloat]
      numVertices = length vs
                           
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

  
  let uv       = [TexCoord2 0.0 0.0
                 ,TexCoord2 1.0 0.0
                 ,TexCoord2 0.0 1.0
                 ,TexCoord2 1.0 0.0
                 ,TexCoord2 1.0 1.0
                 ,TexCoord2 0.0 1.0] :: [TexCoord2 GLfloat]

  textureBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just textureBuffer
  withArray uv $ \ptr -> do
      let size = fromIntegral (numVertices * sizeOf (head uv))
      bufferData ArrayBuffer $= (size, ptr, StaticDraw)
      
  let firstIndex = 0
      uvCoords = AttribLocation 2
  vertexAttribPointer uvCoords $=
      (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset firstIndex))
  vertexAttribArray uvCoords $= Enabled
  
  tx <- loadTex "test.png"
  texture Texture2D $= Enabled
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just tx

  program <- loadShaders [
     ShaderInfo VertexShader (FileSource "shader.vert"),
     ShaderInfo FragmentShader (FileSource "shader.frag")]
  currentProgram $= Just program

  return $ Descriptor triangles firstIndex (fromIntegral numVertices)


loadTex :: FilePath -> IO TextureObject
loadTex f = do t <- either error id <$> readTexture f
               textureFilter Texture2D $= ((Linear', Nothing), Linear')
               texture2DWrap $= (Repeated, ClampToEdge)
               return t                   


resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
    do
      GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0


keyPressed :: GLFW.KeyCallback 
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()


shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()


main :: IO ()
main = do
   GLFW.init
   GLFW.defaultWindowHints
   Just win <- GLFW.createWindow 640 480 "GLFW Demo" Nothing Nothing
   GLFW.makeContextCurrent (Just win)
   GLFW.setWindowSizeCallback win (Just resizeWindow)
   GLFW.setKeyCallback win (Just keyPressed)
   GLFW.setWindowCloseCallback win (Just shutdown)
   descriptor <- initResources
   onDisplay win descriptor
   GLFW.destroyWindow win
   GLFW.terminate


onDisplay :: Window -> Descriptor -> IO ()
onDisplay win descriptor@(Descriptor triangles firstIndex numVertices) = do
  GL.clearColor $= Color4 1 0 0 1
  GL.clear [ColorBuffer]
  bindVertexArrayObject $= Just triangles
  drawArrays Triangles firstIndex numVertices
  GLFW.swapBuffers win
  
  forever $ do
     GLFW.pollEvents
     onDisplay win descriptor
