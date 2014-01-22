module NGL.Rendering where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import NGL.LoadShaders
import NGL.Shape


data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices


bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral


initResources :: [Vertex2 Float] -> IO Descriptor
initResources vs = do
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    let vertices = vs
        numVertices = length vertices

    arrayBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just arrayBuffer
    withArray vertices $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head vertices))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    program <- loadShaders [
        ShaderInfo VertexShader (FileSource "Shaders/triangles.vert"),
        ShaderInfo FragmentShader (FileSource "Shaders/triangles.frac")]
    currentProgram $= Just program

    let firstIndex = 0
        vPosition = AttribLocation 0
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled

    return $ Descriptor triangles firstIndex (fromIntegral numVertices)


resizeWindow :: Size -> IO ()
resizeWindow size@(GL.Size w h) =
    do
        GL.viewport   $= (GL.Position 0 0, size)
        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.ortho2D 0 (realToFrac w) (realToFrac h) 0


createWindow :: String -> (Int, Int) -> IO ()
createWindow title (sizex,sizey) = do
    GLFW.initialize
    GLFW.openWindow (GL.Size 512 512) [] GLFW.Window
    GLFW.windowTitle $= "GLFW Demo"
    GLFW.windowSizeCallback $= resizeWindow
    
drawInWindow :: [[Point]] -> IO ()
drawInWindow vs = do
    descriptor <- initResources $ toVertex2 vs
    onDisplay descriptor
    
closeWindow :: IO ()
closeWindow = do
    GLFW.closeWindow
    GLFW.terminate

onDisplay :: Descriptor -> IO ()
onDisplay descriptor@(Descriptor triangles firstIndex numVertices) = do
    GL.clearColor $= Color4 1 0 0 1
    GL.clear [ColorBuffer]
    bindVertexArrayObject $= Just triangles
    drawArrays Triangles firstIndex numVertices
    GLFW.swapBuffers
    p <- GLFW.getKey GLFW.ESC
    unless (p == GLFW.Press) $ onDisplay descriptor
